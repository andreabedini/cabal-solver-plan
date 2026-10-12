{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Turn a solver result into a build plan: decide how each package is
-- built (per component or as a whole package, as cabal-install 3.14 does),
-- give every unit a deterministic id, keep only what the targets need, and
-- render it as JSON.
--
-- See docs/plan-format.md for the output format.
module Plan (
    PackageSource (..),
    PlanInputs (..),
    Plan,
    makePlan,
    renderPlanJson,
) where

import Control.Monad (forM)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Distribution.CabalSpecVersion (CabalSpecVersion (CabalSpecV1_8))
import Distribution.Client.HashValue (hashValue, showHashValue)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Compat.NonEmptySet qualified as NonEmptySet
import Distribution.Compiler (CompilerInfo (..))
import Distribution.InstalledPackageInfo qualified as IPI
import Distribution.Package (PackageId, PackageIdentifier (..), PackageName, UnitId, mkUnitId, unPackageName, unUnitId)
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
import Distribution.Solver.Modular.Package (I (..), Loc (..), PI (..))
import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.OptionalStanza (enableStanzas)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Solver.Types.PackageIndex qualified as SourceIndex
import Distribution.Solver.Types.PackagePath (PackagePath (..), QPN, Qualified (..), Qualifier (..))
import Distribution.Solver.Types.SolverId (SolverId (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform (..))
import Distribution.Types.Component (Component, componentBuildInfo, componentName)
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

-- | A package as the solver chose it.
data Resolved
    = ResolvedInstalled UnitId PackageId
    | ResolvedSource (SolverPackage PackageSource)

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
    { unitId :: String
    , unitPackageId :: PackageId
    , unitKind :: UnitKind
    , unitDepends :: [UnitKey]
    , unitExeDepends :: [UnitKey]
    , unitSetupDepends :: [UnitKey]
    }

data UnitKind
    = InstalledUnit
        { installedDb :: String
        , installedLibName :: LibraryName
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
    , planTargetUnits :: [String]
    , planPackages :: [(Elaborated, PackageInfo)]
    }

makePlan :: PlanInputs -> [CP QPN] -> IO Plan
makePlan inputs@PlanInputs{..} cps = do
    let resolved = nubOrdOn resolvedKey (map (resolve inputs) cps)
        elaborated =
            Map.fromList
                [ (elabPackageId e, e)
                | ResolvedSource sp <- resolved
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

    let units = makeUnits inputs elaborated infos

        roots =
            nubOrd $
                concat
                    [ case r of
                        ResolvedInstalled uid pkgid
                            | pkgName pkgid `Set.member` planTargets -> [InstalledKey uid]
                        ResolvedSource sp
                            | let pkgid = srcpkgPackageId (solverPkgSource sp)
                            , pkgName pkgid `Set.member` planTargets
                            , Just e <- Map.lookup pkgid elaborated ->
                                sourceKeys e
                        _ -> []
                    | r <- resolved
                    ]

        ordered = dependenciesFirst units roots
        reachable = Set.fromList ordered

    return
        Plan
            { planInputs = inputs
            , planUnits = map (units Map.!) ordered
            , planTargetUnits = map (unitId . (units Map.!)) roots
            , planPackages =
                [ (e, infos Map.! pkgid)
                | (pkgid, e) <- Map.toList elaborated
                , any (`Set.member` reachable) (sourceKeys e)
                ]
            }

-- | Our equivalent of cabal-install-solver's convCP.
resolve :: PlanInputs -> CP QPN -> Resolved
resolve PlanInputs{..} (CP (PI (Q _ pn) (I v loc)) flags stanzas deps) =
    case loc of
        Inst uid -> ResolvedInstalled uid pkgid
        InRepo ->
            ResolvedSource
                SolverPackage
                    { solverPkgSource =
                        fromMaybe (error ("no source package " ++ prettyShow pkgid)) $
                            SourceIndex.lookupPackageId planSourceIndex pkgid
                    , solverPkgFlags = flags
                    , solverPkgStanzas = stanzas
                    , solverPkgLibDeps = fmap fst deps'
                    , solverPkgExeDeps = fmap snd deps'
                    }
  where
    pkgid = PackageIdentifier pn v
    deps' :: ComponentDeps ([SolverId], [SolverId])
    deps' = fmap (partitionEithers . map solverId) deps

    -- Executable dependencies are the ones qualified with the package
    -- itself (the dependencies of those are also qualified).
    solverId (PI (Q (PackagePath _ q) pn') (I v' loc')) =
        let sid = PackageIdentifier pn' v'
         in case loc' of
                Inst uid -> Left (PreExistingId sid uid)
                InRepo
                    | QualExe _ pn'' <- q, pn' == pn'' -> Right (PlannedId sid)
                    | otherwise -> Left (PlannedId sid)

resolvedKey :: Resolved -> Either UnitId PackageId
resolvedKey = \case
    ResolvedInstalled uid _ -> Left uid
    ResolvedSource sp -> Right (srcpkgPackageId (solverPkgSource sp))

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

-- | All units the plan could need, keyed by 'UnitKey'. The map is built
-- lazily since each unit id is a hash of the ids of its dependencies.
makeUnits :: PlanInputs -> Map PackageId Elaborated -> Map PackageId PackageInfo -> Map UnitKey Unit
makeUnits PlanInputs{..} elaborated infos = units
  where
    units = Map.fromList (installedUnits ++ sourceUnits)

    idOf key = maybe (error ("unknown unit " ++ showKey key)) unitId (Map.lookup key units)

    showKey = \case
        InstalledKey uid -> unUnitId uid
        SourceKey pkgid cn -> prettyShow pkgid ++ maybe "" ((":" ++) . prettyShow) cn

    installedUnits =
        [ ( InstalledKey (IPI.installedUnitId ipi)
          , Unit
                { unitId = unUnitId (IPI.installedUnitId ipi)
                , unitPackageId = IPI.sourcePackageId ipi
                , unitKind =
                    InstalledUnit
                        { installedDb = Map.findWithDefault "global" (IPI.installedUnitId ipi) planInstalledDb
                        , installedLibName = IPI.sourceLibName ipi
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
        let unit =
                Unit
                    { unitId = makeUnitId e mcn unit
                    , unitPackageId = elabPackageId e
                    , unitKind = SourceUnit e mcn
                    , unitDepends = depends
                    , unitExeDepends = exeDepends
                    , unitSetupDepends = setupDepends
                    }
            comps = case mcn of
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
         in unit
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

    makeUnitId :: Elaborated -> Maybe ComponentName -> Unit -> String
    makeUnitId e mcn unit = case packageSource info of
        -- The sources of a local package can change at any time, so as
        -- cabal-install does we give it a fixed "inplace" id.
        LocalPackage _ -> prettyShow (elabPackageId e) ++ "-inplace" ++ componentSuffix mcn
        FromRepository _ ->
            prettyShow (elabPackageId e) ++ componentSuffix mcn ++ "-" ++ showHashValue (hashValue (LBS.fromStrict (Text.encodeUtf8 (Text.pack hashInput))))
      where
        info = infos Map.! elabPackageId e
        hashInput =
            unlines
                [ "package: " ++ prettyShow (elabPackageId e)
                , "component: " ++ maybe "" prettyShow mcn
                , "flags: " ++ prettyShow (elabFlags e)
                , "compiler: " ++ prettyShow (compilerInfoId planCompiler)
                , "platform: " ++ prettyShow planPlatform
                , "cabal-file: " ++ maybe "" cabalFileSha256 (packageCabalFile info)
                , "sdist: " ++ maybe "" sdistSha256 (packageSdist info)
                , "depends: " ++ unwords (map idOf (unitDepends unit))
                , "exe-depends: " ++ unwords (map idOf (unitExeDepends unit))
                , "setup-depends: " ++ unwords (map idOf (unitSetupDepends unit))
                ]

    componentSuffix = \case
        Nothing -> ""
        Just (CLibName LMainLibName) -> ""
        Just (CLibName (LSubLibName n)) -> "-l-" ++ prettyShow n
        Just (CFLibName n) -> "-f-" ++ prettyShow n
        Just (CExeName n) -> "-e-" ++ prettyShow n
        Just (CTestName n) -> "-t-" ++ prettyShow n
        Just (CBenchName n) -> "-b-" ++ prettyShow n

-- | The units reachable from the roots, each after its dependencies.
dependenciesFirst :: Map UnitKey Unit -> [UnitKey] -> [UnitKey]
dependenciesFirst units roots = reverse (snd (foldl visit (Set.empty, []) (sortOn keyId roots)))
  where
    keyId k = maybe "" unitId (Map.lookup k units)
    visit (seen, acc) k
        | k `Set.member` seen = (seen, acc)
        | otherwise =
            let unit = fromMaybe (error "dependenciesFirst: unknown unit") (Map.lookup k units)
                deps = sortOn keyId (nubOrd (unitDepends unit ++ unitExeDepends unit ++ unitSetupDepends unit))
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
            [ "format" .= (1 :: Int)
            , "compiler-id" .= prettyShow (compilerInfoId planCompiler)
            , "os" .= prettyShow os
            , "arch" .= prettyShow arch
            , "index-state" .= Map.fromList [(name, prettyShow ts) | (name, ts) <- indexStates]
            , "targets" .= planTargetUnits
            , "packages" .= map packageJson planPackages
            , "units" .= map unitJson planUnits
            ]
  where
    Platform arch os = planPlatform

    config =
        Aeson.defConfig
            { Aeson.confIndent = Aeson.Spaces 2
            , Aeson.confCompare = Aeson.keyOrder keyOrder <> compare
            , Aeson.confTrailingNewline = True
            }
    keyOrder =
        [ "format"
        , "compiler-id"
        , "os"
        , "arch"
        , "index-state"
        , "targets"
        , "packages"
        , "units"
        , "id"
        , "type"
        , "package"
        , "name"
        , "version"
        , "build-type"
        , "build-style"
        , "component"
        , "components"
        , "db"
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

    ids = Map.fromList [(unitKey u, unitId u) | u <- planUnits]
    unitKey u = case unitKind u of
        InstalledUnit{} -> InstalledKey (mkUnitId (unitId u))
        SourceUnit e mcn -> SourceKey (elabPackageId e) mcn
    idsOf keys = mapMaybe (`Map.lookup` ids) keys

    packageJson (e, PackageInfo{..}) =
        object
            [ "id" .= prettyShow (elabPackageId e)
            , "name" .= unPackageName (pkgName (elabPackageId e))
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
            [ "id" .= unitId u
            , "type" .= ("pre-existing" :: String)
            , "name" .= unPackageName (pkgName (unitPackageId u))
            , "version" .= prettyShow (pkgVersion (unitPackageId u))
            , "component" .= prettyShow (CLibName installedLibName)
            , "db" .= installedDb
            , "depends" .= idsOf (unitDepends u)
            ]
        SourceUnit e mcn ->
            [ "id" .= unitId u
            , "type" .= ("configured" :: String)
            , "package" .= prettyShow (elabPackageId e)
            , "name" .= unPackageName (pkgName (unitPackageId u))
            , "version" .= prettyShow (pkgVersion (unitPackageId u))
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
