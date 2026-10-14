{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Turn a solver result into a build plan: decide how each package is
-- built (per component or as a whole package, as cabal-install 3.14 does),
-- keep only what the targets need, and render it as JSON.
--
-- See docs/plan-format.md for the output format.
module Plan (
    PackageSource (..),
    PlanInputs (..),
    Plan,
    makePlan,
    unsupportedUnits,
    renderPlanJson,
) where

import Control.Monad (forM)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Distribution.CabalSpecVersion (CabalSpecVersion (CabalSpecV1_8))
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Compat.NonEmptySet qualified as NonEmptySet
import Distribution.Compiler (CompilerInfo (..))
import Distribution.InstalledPackageInfo qualified as IPI
import Distribution.Package (PackageId, PackageIdentifier (..), PackageName, UnitId, unPackageName, unUnitId)
import Distribution.PackageDescription (
    BuildType (..),
    ComponentName (..),
    FlagAssignment,
    LibraryName (..),
    PackageDescription,
    enabledComponents,
    flagDefault,
    flagName,
    genPackageFlags,
    mkFlagAssignment,
    unFlagAssignment,
    unFlagName,
 )
import Distribution.PackageDescription qualified as PD
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as InstalledIndex
import Distribution.Solver.Modular.Configured (CP (..))
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Package (I (..), Loc (..), PI (..))
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.OptionalStanza (enableStanzas)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Solver.Types.PackagePath (QPN, Qualified (..), showQPN)
import Distribution.Solver.Types.ResolverPackage (ResolverPackage (..))
import Distribution.Solver.Types.SolverId (SolverId (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform (..))
import Distribution.Types.Component (Component (CLib), componentBuildInfo, componentName)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.DependencySatisfaction (DependencySatisfaction (Satisfied))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.VersionRange (thisVersion)
import IndexTar (CabalFileInfo (..), Index (..), Repository (..), SdistInfo (..), cabalFileInfo, sdistInfo)

-- | Where a source package comes from.
data PackageSource
    = FromRepository Repository
    | -- | A local package, in the given directory.
      LocalPackage FilePath
    deriving (Eq, Show)

data PlanInputs = PlanInputs
    { planCompiler :: CompilerInfo
    , planPlatform :: Platform
    , planInstalled :: InstalledPackageIndex
    , planInstalledDb :: Map UnitId String
    -- ^ Which package db each installed unit comes from ("global" or a path).
    , planSourceIndex :: PackageIndex (SourcePackage PackageSource)
    , planIndexes :: [Index]
    , planTargets :: Set PackageName
    }

data BuildStyle = PerComponent | WholePackage
    deriving (Eq)

-- | A source package, finalised with the flags and stanzas the solver chose.
data Elaborated = Elaborated
    { elabSolverPackage :: SolverPackage PackageSource
    , elabPackageId :: PackageId
    , elabFlags :: FlagAssignment
    -- ^ Every flag of the package, defaults included.
    , elabDescription :: PackageDescription
    , elabComponents :: [Component]
    -- ^ The enabled, buildable components.
    , elabBuildStyle :: BuildStyle
    }

data UnitKey
    = InstalledKey UnitId
    | -- | A component of a package, or the whole package ('Nothing').
      SourceKey PackageId (Maybe ComponentName)
    deriving (Eq, Ord)

data Unit = Unit
    { unitPackageId :: PackageId
    , unitKind :: UnitKind
    , unitDepends :: [UnitKey]
    , unitExeDepends :: [UnitKey]
    , unitSetupDepends :: [UnitKey]
    }

data UnitKind
    = InstalledUnit
        { installedUnitId :: UnitId
        , installedDb :: String
        , installedLibName :: LibraryName
        , installedIndefinite :: Bool
        }
    | -- | A component of a package, or the whole package ('Nothing').
      SourceUnit Elaborated (Maybe ComponentName)

data PackageInfo = PackageInfo
    { packageSource :: PackageSource
    , packageCabalFile :: Maybe CabalFileInfo
    , packageSdist :: Maybe SdistInfo
    }

data Plan = Plan
    { planInputs :: PlanInputs
    , planUnits :: [Unit]
    -- ^ Dependencies first.
    , planTargetUnits :: [UnitKey]
    , planQualifiedNames :: Map UnitKey [String]
    -- ^ The qualified names the solver chose each unit's package under.
    , planPackages :: [(Elaborated, PackageInfo)]
    }

makePlan :: PlanInputs -> [CP QPN] -> IO Plan
makePlan inputs@PlanInputs{..} cps = do
    -- convCP expects every installed choice in planInstalled; Main refuses
    -- plans with --with-preinstalled packages, the only ones that are not.
    let resolvedAll = map (convCP planInstalled planSourceIndex) cps
        -- Merge the qualified names that chose the same package, as
        -- cabal-install's modularResolver does. The solver's single instance
        -- restriction makes all but one of them link to the other, and
        -- linked choices have the same flags, stanzas and dependencies.
        resolved = nubOrdOn resolvedKey resolvedAll

        configuration sp = (solverPkgFlags sp, solverPkgStanzas sp, solverPkgLibDeps sp, solverPkgExeDeps sp)
        configurations =
            Map.fromListWith
                Set.union
                [ (srcpkgPackageId (solverPkgSource sp), Set.singleton (configuration sp))
                | Configured sp <- resolvedAll
                ]
    case [pkgid | (pkgid, cs) <- Map.toList configurations, Set.size cs > 1] of
        [] -> return ()
        pkgids -> error ("the solver chose several configurations of " ++ unwords (map prettyShow pkgids))

    let elaborated =
            Map.fromList
                [ (elabPackageId e, e)
                | Configured sp <- resolved
                , let e = elaborate inputs sp
                ]

    infos <- fmap Map.fromList $ forM (Map.elems elaborated) $ \e -> do
        let pkgid = elabPackageId e
            source = srcpkgSource (solverPkgSource (elabSolverPackage e))
            index = case source of
                FromRepository repo -> find ((== repo) . indexRepository) planIndexes
                LocalPackage _ -> Nothing
        cabalFile <- maybe (pure Nothing) (`cabalFileInfo` pkgid) index
        sdist <- maybe (pure Nothing) (`sdistInfo` pkgid) index
        return (pkgid, PackageInfo source cabalFile sdist)

    let units = makeUnits inputs elaborated

        roots =
            nubOrd $
                concat
                    [ case r of
                        PreExisting isp
                            | let ipi = instSolverPkgIPI isp
                            , pkgName (IPI.sourcePackageId ipi) `Set.member` planTargets ->
                                [InstalledKey (IPI.installedUnitId ipi)]
                        Configured sp
                            | let pkgid = srcpkgPackageId (solverPkgSource sp)
                            , pkgName pkgid `Set.member` planTargets
                            , Just e <- Map.lookup pkgid elaborated ->
                                sourceKeys e
                        _ -> []
                    | r <- resolved
                    ]

        ordered = dependenciesFirst units roots

        -- The solver can choose the same package under several qualifiers
        -- (say @base@ and @network:hsc2hs:exe.base@); its units have them all.
        qualifiedNames =
            Map.map Set.toAscList $
                Map.fromListWith
                    Set.union
                    [ (key, Set.singleton (showQPN qpn))
                    | CP (PI qpn@(Q _ pn) (I v loc)) _ _ _ <- cps
                    , let pkgid = PackageIdentifier pn v
                    , key <- case loc of
                        -- An installed package's sublibraries are separate
                        -- units, found by package id as in 'unitOf'.
                        Inst uid ->
                            InstalledKey uid
                                : [ InstalledKey (IPI.installedUnitId ipi)
                                  | ipi <- InstalledIndex.lookupSourcePackageId planInstalled pkgid
                                  , LSubLibName _ <- [IPI.sourceLibName ipi]
                                  ]
                        InRepo -> maybe [] sourceKeys (Map.lookup pkgid elaborated)
                    ]
        reachable = Set.fromList ordered

    return
        Plan
            { planInputs = inputs
            , planUnits = map (units Map.!) ordered
            , planTargetUnits = roots
            , planQualifiedNames = qualifiedNames
            , planPackages =
                [ (e, infos Map.! pkgid)
                | (pkgid, e) <- Map.toList elaborated
                , any (`Set.member` reachable) (sourceKeys e)
                ]
            }

-- | The units of a plan that we cannot describe correctly, with the reason.
--
-- Backpack is not supported: an indefinite library (one with signatures)
-- has to be instantiated, which gives extra units that 'makePlan' does not
-- make. Rather than give a plan that fails to build, we refuse it.
unsupportedUnits :: Plan -> [(String, String)]
unsupportedUnits Plan{..} =
    [ (showKey (unitKey u), reason)
    | u <- planUnits
    , reason <- case unitKind u of
        InstalledUnit{installedIndefinite = True} ->
            ["it is an indefinite (Backpack) library"]
        InstalledUnit{} -> []
        SourceUnit e mcn ->
            [ "its " ++ prettyShow (componentName c) ++ " has Backpack signatures: " ++ unwords (map prettyShow sigs)
            | c@(CLib lib) <- elabComponents e
            , maybe True (== componentName c) mcn
            , let sigs = PD.signatures lib
            , not (null sigs)
            ]
    ]

resolvedKey :: ResolverPackage PackageSource -> Either UnitId PackageId
resolvedKey = \case
    PreExisting isp -> Left (IPI.installedUnitId (instSolverPkgIPI isp))
    Configured sp -> Right (srcpkgPackageId (solverPkgSource sp))

elaborate :: PlanInputs -> SolverPackage PackageSource -> Elaborated
elaborate PlanInputs{..} sp =
    Elaborated
        { elabSolverPackage = sp
        , elabPackageId = pkgid
        , elabFlags = flags
        , elabDescription = pd
        , elabComponents = components
        , elabBuildStyle =
            -- cabal-install 3.14 builds these as a whole package. (3.18 builds
            -- Configure packages per component, but timbuktu's per-component
            -- build cannot run a configure script.)
            if PD.buildType pd `elem` [Configure, Custom, Hooks]
                || PD.specVersion pd < CabalSpecV1_8
                || null components
                then WholePackage
                else PerComponent
        }
  where
    srcpkg = solverPkgSource sp
    pkgid = srcpkgPackageId srcpkg
    gpd = srcpkgDescription srcpkg
    flags =
        mkFlagAssignment
            $ Map.toList
            $ Map.fromList (unFlagAssignment (solverPkgFlags sp))
                `Map.union` Map.fromList [(flagName f, flagDefault f) | f <- genPackageFlags gpd]
    enabled = enableStanzas (solverPkgStanzas sp)
    pd = case finalizePD flags enabled (const Satisfied) planPlatform planCompiler [] gpd of
        Right (pd', _) -> pd'
        Left missing -> error ("cannot finalise " ++ prettyShow pkgid ++ ", missing " ++ unwords (map prettyShow missing))
    components = enabledComponents pd enabled

sourceKeys :: Elaborated -> [UnitKey]
sourceKeys e = case elabBuildStyle e of
    WholePackage -> [SourceKey (elabPackageId e) Nothing]
    PerComponent -> [SourceKey (elabPackageId e) (Just (componentName c)) | c <- elabComponents e]

unitKey :: Unit -> UnitKey
unitKey u = case unitKind u of
    InstalledUnit{installedUnitId} -> InstalledKey installedUnitId
    SourceUnit e mcn -> SourceKey (elabPackageId e) mcn

-- | A unit's name for messages: the unit id of a pre-existing unit, or
-- @<package id>[:<component>]@.
showKey :: UnitKey -> String
showKey = \case
    InstalledKey uid -> unUnitId uid
    SourceKey pkgid cn -> prettyShow pkgid ++ maybe "" ((":" ++) . prettyShow) cn

-- | All units the plan could need, keyed by 'UnitKey'.
makeUnits :: PlanInputs -> Map PackageId Elaborated -> Map UnitKey Unit
makeUnits PlanInputs{..} elaborated = Map.fromList (installedUnits ++ sourceUnits)
  where
    installedUnits =
        [ ( InstalledKey (IPI.installedUnitId ipi)
          , Unit
                { unitPackageId = IPI.sourcePackageId ipi
                , unitKind =
                    InstalledUnit
                        { installedUnitId = IPI.installedUnitId ipi
                        , installedDb = Map.findWithDefault "global" (IPI.installedUnitId ipi) planInstalledDb
                        , installedLibName = IPI.sourceLibName ipi
                        , installedIndefinite = IPI.indefinite ipi
                        }
                , unitDepends = map InstalledKey (IPI.depends ipi)
                , unitExeDepends = []
                , unitSetupDepends = []
                }
          )
        | ipi <- InstalledIndex.allPackages planInstalled
        ]

    sourceUnits =
        [ (key, sourceUnit e key)
        | e <- Map.elems elaborated
        , key <- sourceKeys e
        ]

    sourceUnit e (SourceKey _ mcn) =
        let comps = case mcn of
                Nothing -> elabComponents e
                Just cn -> filter ((== cn) . componentName) (elabComponents e)
            -- In a whole-package build the dependencies between the
            -- package's own components are not units.
            external = case mcn of
                Nothing -> filter (not . isOwn)
                Just _ -> id
            isOwn = \case
                SourceKey pkgid _ -> pkgid == elabPackageId e
                InstalledKey _ -> False
            depends = external $ nubOrd (concatMap (libDependencies e) comps)
            exeDepends = external $ nubOrd (concatMap (exeDependencies e) comps)
            setupDepends = case elabBuildStyle e of
                WholePackage ->
                    nubOrd
                        [ k
                        | sid <- ComponentDeps.setupDeps (solverPkgLibDeps (elabSolverPackage e))
                        , k <- unitOf sid (CLibName LMainLibName)
                        ]
                PerComponent -> []
         in Unit
                { unitPackageId = elabPackageId e
                , unitKind = SourceUnit e mcn
                , unitDepends = depends
                , unitExeDepends = exeDepends
                , unitSetupDepends = setupDepends
                }
    sourceUnit _ key = error ("not a source unit: " ++ showKey key)

    -- The units a component's build-depends refer to.
    libDependencies e c =
        [ k
        | Dependency pn _ libs <- PD.targetBuildDepends (componentBuildInfo c)
        , lib <- NonEmptySet.toList libs
        , k <-
            if pn == pkgName (elabPackageId e)
                then [ownUnit e (CLibName lib)]
                else case find ((== pn) . pkgName . solverSrcId) (solverDeps solverPkgLibDeps e c) of
                    Just sid -> unitOf sid (CLibName lib)
                    Nothing -> error (prettyShow (elabPackageId e) ++ ": no solver choice for dependency " ++ prettyShow pn)
        ]

    -- The units of the executables a component's build-tool-depends refer to.
    exeDependencies e c =
        [ k
        | ExeDependency pn exe _ <- getAllToolDependencies (elabDescription e) (componentBuildInfo c)
        , k <-
            if pn == pkgName (elabPackageId e)
                then [ownUnit e (CExeName exe)]
                else case find ((== pn) . pkgName . solverSrcId) (solverDeps solverPkgExeDeps e c) of
                    Just sid -> unitOf sid (CExeName exe)
                    -- e.g. a legacy build-tools entry the solver does not know about
                    Nothing -> []
        ]

    solverDeps field e c =
        ComponentDeps.select (== ComponentDeps.componentNameToComponent (componentName c)) (field (elabSolverPackage e))

    ownUnit e cn = case elabBuildStyle e of
        WholePackage -> SourceKey (elabPackageId e) Nothing
        PerComponent -> SourceKey (elabPackageId e) (Just cn)

    -- The unit providing a component of a package the solver chose.
    unitOf :: SolverId -> ComponentName -> [UnitKey]
    unitOf sid cn = case sid of
        PreExistingId pkgid uid -> case cn of
            CLibName LMainLibName -> [InstalledKey uid]
            CLibName lib ->
                [ InstalledKey (IPI.installedUnitId ipi)
                | (_, ipis) <- InstalledIndex.lookupInternalDependency planInstalled (pkgName pkgid) (thisVersion (pkgVersion pkgid)) lib
                , ipi <- take 1 ipis
                ]
            _ -> []
        PlannedId pkgid -> case Map.lookup pkgid elaborated of
            Just e -> [ownUnit e cn]
            Nothing -> error ("not in the plan: " ++ prettyShow pkgid)

-- | The units reachable from the roots, each after its dependencies.
dependenciesFirst :: Map UnitKey Unit -> [UnitKey] -> [UnitKey]
dependenciesFirst units roots = reverse (snd (foldl visit (Set.empty, []) (sortNub roots)))
  where
    sortNub = Set.toAscList . Set.fromList
    visit (seen, acc) k
        | k `Set.member` seen = (seen, acc)
        | otherwise =
            let unit = fromMaybe (error "dependenciesFirst: unknown unit") (Map.lookup k units)
                deps = sortNub (unitDepends unit ++ unitExeDepends unit ++ unitSetupDepends unit)
                (seen', acc') = foldl visit (Set.insert k seen, acc) deps
             in (seen', k : acc')

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | f x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert (f x) seen) xs

renderPlanJson :: [(String, Timestamp)] -> Plan -> LBS.ByteString
renderPlanJson indexStates Plan{planInputs = PlanInputs{..}, ..} =
    Aeson.encodePretty' config $
        object
            [ "format" .= (3 :: Int)
            , "compiler-id" .= prettyShow (compilerInfoId planCompiler)
            , "os" .= prettyShow os
            , "arch" .= prettyShow arch
            , "index-state" .= Map.fromList [(name, prettyShow ts) | (name, ts) <- indexStates]
            , "targets" .= idsOf planTargetUnits
            , "packages" .= Map.fromList [(prettyShow (elabPackageId e), packageJson p) | p@(e, _) <- planPackages]
            , "units" .= Map.fromList [(ids Map.! unitKey u, unitJson u) | u <- planUnits]
            ]
  where
    Platform arch os = planPlatform

    config =
        Aeson.defConfig
            { Aeson.confIndent = Aeson.Spaces 2
            , Aeson.confCompare = Aeson.keyOrder keyOrder <> numerically <> compare
            , Aeson.confTrailingNewline = True
            }
    -- Unit ids sort as numbers: a longer string of digits is a larger number.
    numerically a b
        | isNumber a, isNumber b = compare (Text.length a) (Text.length b)
        | otherwise = EQ
    isNumber k = not (Text.null k) && Text.all isDigit k
    keyOrder =
        [ "format"
        , "compiler-id"
        , "os"
        , "arch"
        , "index-state"
        , "targets"
        , "packages"
        , "units"
        , "type"
        , "package"
        , "name"
        , "version"
        , "build-type"
        , "build-style"
        , "qualified-names"
        , "component"
        , "components"
        , "db"
        , "unit-id"
        , "flags"
        , "src"
        , "cabal-file"
        , "repo"
        , "path"
        , "revision"
        , "sha256"
        , "size"
        , "depends"
        , "exe-depends"
        , "setup-depends"
        ]

    -- Unit ids are only keys. Numbering the units dependencies first, and
    -- printing them in numerical order, keeps the output readable.
    ids = Map.fromList (zip (map unitKey planUnits) (map show [0 :: Int ..]))
    idsOf keys = mapMaybe (`Map.lookup` ids) keys
    qualifiedNamesOf u = Map.findWithDefault [] (unitKey u) planQualifiedNames

    packageJson (e, PackageInfo{..}) =
        object
            [ "name" .= unPackageName (pkgName (elabPackageId e))
            , "version" .= prettyShow (pkgVersion (elabPackageId e))
            , "build-type" .= prettyShow (PD.buildType (elabDescription e))
            , "src" .= case packageSource of
                FromRepository repo ->
                    object
                        [ "type" .= ("repo-tar" :: String)
                        , "repo" .= show (repositoryUri repo)
                        , "sha256" .= fmap sdistSha256 packageSdist
                        , "size" .= fmap sdistSize packageSdist
                        ]
                LocalPackage path ->
                    object
                        [ "type" .= ("local" :: String)
                        , "path" .= path
                        ]
            , "cabal-file"
                .= fmap
                    ( \CabalFileInfo{..} ->
                        object
                            [ "revision" .= cabalFileRevision
                            , "sha256" .= cabalFileSha256
                            , "size" .= cabalFileSize
                            ]
                    )
                    packageCabalFile
            ]

    unitJson :: Unit -> Value
    unitJson u = object $ case unitKind u of
        InstalledUnit{..} ->
            [ "type" .= ("pre-existing" :: String)
            , "name" .= unPackageName (pkgName (unitPackageId u))
            , "version" .= prettyShow (pkgVersion (unitPackageId u))
            , "qualified-names" .= qualifiedNamesOf u
            , "component" .= prettyShow (CLibName installedLibName)
            , "db" .= installedDb
            , "unit-id" .= unUnitId installedUnitId
            , "depends" .= idsOf (unitDepends u)
            ]
        SourceUnit e mcn ->
            [ "type" .= ("configured" :: String)
            , "package" .= prettyShow (elabPackageId e)
            , "qualified-names" .= qualifiedNamesOf u
            , "flags" .= Map.fromList [(unFlagName f, b) | (f, b) <- unFlagAssignment (elabFlags e)]
            , "depends" .= idsOf (unitDepends u)
            , "exe-depends" .= idsOf (unitExeDepends u)
            , "setup-depends" .= idsOf (unitSetupDepends u)
            ]
                ++ case mcn of
                    Just cn ->
                        [ "build-style" .= ("per-component" :: String)
                        , "component" .= prettyShow cn
                        ]
                    Nothing ->
                        [ "build-style" .= ("whole-package" :: String)
                        , "components" .= map (prettyShow . componentName) (elabComponents e)
                        ]
