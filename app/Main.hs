{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.List (dropWhileEnd, intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Traversable (for)
import Distribution.Client.Dependency (PackageProperty (..))
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils (TotalIndexState)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), lookupIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Types (
    PackageSpecifier (..),
    RemoteRepo (..),
    Repo (..),
    RepoName (..),
    emptyRemoteRepo,
    pkgSpecifierTarget,
 )
import Distribution.InstalledPackageInfo (InstalledPackageInfo, installedUnitId, parseInstalledPackageInfo)
import Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty qualified as Cabal
import Distribution.Simple (CompilerInfo, PackageDBX (..), compilerInfo)
import Distribution.Simple.Errors (exceptionMessage)
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Utils (VerboseException (..))
import Distribution.Solver.Modular.Configured (CP (..))
import Distribution.Solver.Modular.Message (renderSummarizedMessage)
import Distribution.Solver.Modular.Package (I (..), Loc (..), PI (..), showPI)
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.OptionalStanza (showStanzas)
import Distribution.Solver.Types.PackageIndex qualified as SourceIndex
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..))
import Distribution.Solver.Types.PkgConfigDb qualified as Solver
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform (..), buildPlatform)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Verbosity (Verbosity)
import Distribution.Version (isAnyVersion)
import Hackage.Security.Client qualified as Sec
import IndexTar (Index (..), Repository (..), indexSourcePackages, readIndex)
import Log
import Network.URI (URI (..), URIAuth (..))
import Options
import Plan (PackageSource (..), PlanInputs (..), makePlan, renderPlanJson, unsupportedUnits)
import SetupDeps (applyDefaultSetupDeps)
import SolverInterface (solve)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, exeExtension, findExecutable, getXdgDirectory, listDirectory)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (isPathSeparator, takeDirectory, takeExtension, (<.>), (</>))
import System.IO.Error (ioeGetErrorString, isUserError)
import Prelude hiding (pi)

-- | Exit codes: 0 a plan was found, 1 no plan was found, 2 usage error
-- (see "Options"), 3 any other error.
main :: IO ()
main = do
    options <- parseOptions
    -- Everything but the plan goes to stderr.
    let logger = stderrLogger (logLevel options)
    run logger options `catch` reportError logger

-- | Report an error that stops the program, for a human: no exception
-- types or backtraces.
reportError :: Logger -> SomeException -> IO ()
reportError logger e
    | isExit e = throwIO e
    | otherwise = do
        logger <& Fatal (errorMessage e)
        exitWith (ExitFailure 3)

-- | Say what was being done when an error happened.
inContext :: String -> IO a -> IO a
inContext context action =
    action `catch` \e ->
        if isExit e then throwIO e else fail (context ++ ": " ++ errorMessage e)

-- | An exit or an interrupt, rather than an error.
isExit :: SomeException -> Bool
isExit e =
    isJust (fromException e :: Maybe ExitCode)
        || isJust (fromException e :: Maybe SomeAsyncException)

errorMessage :: SomeException -> String
errorMessage e = dropWhileEnd (== '\n') $ case () of
    _
        | Just ioe <- fromException e, isUserError ioe -> ioeGetErrorString ioe
        | Just (VerboseException _ _ _ cabalError) <- fromException e -> exceptionMessage cabalError
        | otherwise -> displayException e

run :: Logger -> Options -> IO ()
run logger Options{..} = do
    let verbosity = cabalVerbosity logLevel

    for_ deprecations ((logger <&) . Deprecated)

    -- Check what can be checked before doing any slow work.
    checkInputs compilerSource extraPackageDbs indexTar localPackages

    -- Here we interpret the user input w.r.t to the compiler, platform and
    -- package databases. While it is possible to specify these pieces
    -- idependently; they lose their independence if we let the user refer to
    -- an installed version of GHC. So there are two cases here: with or
    -- without access to a real compiler.
    (cinfo, platform@(Platform arch os), globalIPI) <-
        determineCompiler verbosity compilerSource

    extraIPIs <- for extraPackageDbs $ \path -> (,) path <$> readPackageDb logger path
    let installedPkgIndex0 = globalIPI <> foldMap snd extraIPIs
        -- Which package db each installed unit comes from.
        installedDb =
            Map.fromList
                [ (installedUnitId ipi, path)
                | (path, index) <- extraIPIs
                , ipi <- Cabal.Index.allPackages index
                ]

    pkgConfigDb <- do
        let cliPkgDb =
                if null pkgConfigDbEntries
                    then Nothing
                    else Just (Map.fromList pkgConfigDbEntries)
        if useSystemPkgConfigDb
            then
                Solver.readPkgConfigDb verbosity defaultProgramDb >>= \case
                    Just (PkgConfigDb m) -> pure (Just m <> cliPkgDb)
                    Nothing -> pure cliPkgDb
            else
                pure cliPkgDb

    indexes <- do
        cacheDir' <- maybe (getXdgDirectory XdgCache "cabal-solver-plan") return cacheDir
        fromRepositories <- loadRepositories logger verbosity repositories cacheDir' offline
        let fromIndex =
                [ Repository
                    { repositoryName = maybe "index" uriRegName (uriAuthority indexUri)
                    , repositoryUri = indexUri
                    , repositoryIndexTar = path
                    }
                | Just path <- [indexTar]
                ]
        for (fromIndex ++ fromRepositories) $ \repo -> do
            logger <& ReadingIndex (repositoryName repo) (repositoryIndexTar repo)
            index <-
                inContext ("cannot read the index " ++ repositoryIndexTar repo) $
                    readIndex repo (indexStateOf repo mTotalIndexState)
            logger
                <& IndexRead
                    (repositoryName repo)
                    (Map.size (indexPackages index))
                    (Cabal.prettyShow (indexTimestamp index))
            return index

    locals <- traverse readLocalPackage localPackages

    -- Local packages replace the index packages with the same id, and
    -- installed ones are hidden (as cabal-install's basicInstallPolicy).
    -- As cabal-install does, give Custom and Hooks packages without a
    -- custom-setup stanza default setup dependencies.
    sourcePkgIndex <- do
        fromIndexes <- mconcat <$> traverse indexSourcePackages indexes
        let fromRepositories = fmap (\sp -> sp{srcpkgSource = FromRepository (srcpkgSource sp)}) fromIndexes
        return
            $ fmap (applyDefaultSetupDeps cinfo platform)
            $ foldl (flip SourceIndex.insert) fromRepositories locals
    let installedPkgIndex = foldl (flip Cabal.Index.deleteSourcePackageId) installedPkgIndex0 (map srcpkgPackageId locals)
    let sourcePkgPrefs = Map.unionsWith (const id) (map indexPreferences indexes)

    -- As cabal-install's pkgSpecifierConstraints, a target's version range
    -- constrains it, and a local package is pinned to its own version.
    let specifiers =
            [ NamedPackage pn [PackagePropertyVersion vr | not (isAnyVersion vr)]
            | PackageVersionConstraint pn vr <- targets
            ]
                ++ map SpecificSourcePackage locals
        targetsPackageName = Set.fromList (map pkgSpecifierTarget specifiers)

        progress =
            solve
                cinfo
                os
                arch
                (PkgConfigDb <$> pkgConfigDb)
                constraints
                preferences
                flagAssignments
                solverConfig
                preferOldest
                installedPkgIndex
                sourcePkgIndex
                sourcePkgPrefs
                extraPreInstalled
                specifiers

    result <-
        foldProgress
            -- step
            (\step rest -> (logger <& SolverStep (renderSummarizedMessage step)) >> rest)
            -- fail
            (return . Left)
            -- done
            (return . Right)
            progress

    case result of
        Left failure -> do
            let hints =
                    [ "No package index was given, so only installed packages can be used: add --repository or --index."
                    | null indexes
                    ]
                        ++ [ "No packages are installed, so even base would have to be built from source: "
                                ++ case compilerSource of
                                    CompilerInline{} -> "give the compiler's package db with --package-db."
                                    CompilerFromSystem{} -> "leave out --no-global-package-db, or give --package-db."
                           | null (Cabal.Index.allPackages installedPkgIndex) && null extraPreInstalled
                           ]
            logger <& NoPlan (dropWhile (== '\n') (dropWhileEnd (== '\n') failure)) hints
            exitFailure
        Right cps
            | jsonOutput -> do
                -- A --with-preinstalled package is not in any package db,
                -- so the plan cannot say where to find it.
                case nubOrd [showPI pi | CP pi@(PI _ (I _ (Inst uid))) _ _ _ <- cps, isNothing (Cabal.Index.lookupUnitId installedPkgIndex uid)] of
                    [] -> return ()
                    pis -> do
                        logger
                            <& PlanRefused
                                "Cannot make a plan, it uses packages from --with-preinstalled, which --json does not support:"
                                pis
                        exitFailure
                plan <-
                    makePlan
                        PlanInputs
                            { planCompiler = cinfo
                            , planPlatform = platform
                            , planInstalled = installedPkgIndex
                            , planInstalledDb = installedDb
                            , planSourceIndex = sourcePkgIndex
                            , planIndexes = indexes
                            , planTargets = targetsPackageName
                            }
                        cps
                case unsupportedUnits plan of
                    [] -> return ()
                    units -> do
                        logger
                            <& PlanRefused
                                "Cannot make a plan, Backpack is not supported:"
                                [uid ++ ": " ++ reason | (uid, reason) <- units]
                        exitFailure
                LBS.putStr $
                    renderPlanJson
                        [(repositoryName (indexRepository i), indexTimestamp i) | i <- indexes]
                        plan
            | otherwise ->
                for_ cps $ \(CP pi flagAssignment optionalStanzaSet componentDeps) ->
                    putStrLn
                        $ unlines
                        $ unwords
                            [ showPI pi
                            , Cabal.prettyShow flagAssignment
                            , showStanzas optionalStanzaSet
                            ]
                            : map
                                ( \g ->
                                    Cabal.prettyShow (fst (NE.head g))
                                        ++ "\t"
                                        ++ intercalate ", " (foldMap (map showPI . snd) g)
                                )
                                (NE.groupWith fst $ ComponentDeps.toList componentDeps)

-- | Check that the inputs exist, before the slow work starts.
checkInputs :: CompilerSource -> [FilePath] -> Maybe FilePath -> [FilePath] -> IO ()
checkInputs compilerSource packageDbs indexTar localPackages = do
    problems <-
        catMaybes
            <$> sequence
                ( [ compilerProblem hcPath
                  | CompilerFromSystem hcPath _ <- [compilerSource]
                  ]
                    ++ [ problemUnless doesDirectoryExist ("--package-db " ++ path ++ ": no such directory") path
                       | path <- packageDbs
                       ]
                    ++ [ problemUnless doesFileExist ("--index " ++ path ++ ": no such file") path
                       | Just path <- [indexTar]
                       ]
                    ++ [ problemUnless doesPathExist ("--local-package " ++ path ++ ": no such file or directory") path
                       | path <- localPackages
                       ]
                )
    case problems of
        [] -> return ()
        [problem] -> fail problem
        _ -> fail (intercalate "\n" ("cannot read the inputs:" : map ("  " ++) problems))
  where
    problemUnless exists problem path = do
        ok <- exists path
        return (if ok then Nothing else Just problem)
    compilerProblem hcPath
        | any isPathSeparator hcPath = do
            ok <- or <$> traverse doesFileExist [hcPath, hcPath <.> exeExtension]
            return $
                if ok then Nothing else Just ("--with-compiler " ++ hcPath ++ ": no such file")
        | otherwise = do
            found <- findExecutable hcPath
            return $ case found of
                Just _ -> Nothing
                Nothing -> Just ("cannot find " ++ hcPath ++ " on the PATH: install GHC, or give its path with --with-compiler")

determineCompiler ::
    Verbosity ->
    CompilerSource ->
    IO (CompilerInfo, Platform, InstalledPackageIndex)
determineCompiler verbosity = \case
    CompilerInline cinfo platform -> do
        return (cinfo, platform, mempty)
    CompilerFromSystem hcPath useGlobalPackageDb -> do
        (compiler, mPlatform, programdb) <-
            GHC.configure verbosity (Just hcPath) Nothing defaultProgramDb
        let platform = fromMaybe buildPlatform mPlatform
        installedPkgIndex <-
            if useGlobalPackageDb
                then GHC.getInstalledPackages verbosity Nothing [GlobalPackageDB] programdb
                else mempty
        return (compilerInfo compiler, platform, installedPkgIndex)

-- | The index-state to use for a repository, if any.
indexStateOf :: Repository -> Maybe TotalIndexState -> Maybe Timestamp
indexStateOf repo = \case
    Nothing -> Nothing
    Just tis -> case lookupIndexState (RepoName (repositoryName repo)) tis of
        IndexStateHead -> Nothing
        IndexStateTime ts -> Just ts

-- | Find the package indexes of source package repositories.
--
-- The user can specify a repository URI on the command line. Unless
-- --offline is given, we use hackage-security (through cabal-install) to
-- download or update the repository in a local cache directory. Either way
-- we then read the index (01-index.tar) from the cache directory ourselves.
--
-- Only secure repositories are supported. Note that secure repositories
-- do not have to be remote but can also be local, use a URI with file:
-- scheme.
loadRepositories ::
    Logger ->
    Verbosity ->
    [Either URI RemoteRepo] ->
    FilePath ->
    Bool ->
    IO [Repository]
loadRepositories logger verbosity repositories cacheDir offline = do
    unless (offline || null remoteRepos) $ do
        -- Note: this is workaround for a bug in cabal-install where it
        -- tries to create hackage-security-lock in repoLocalDir, without
        -- making sure that directory exists.
        for_ remoteRepos $ \RemoteRepo{remoteRepoName} ->
            createDirectoryIfMissing True (cacheDir </> unRepoName remoteRepoName)

        withRepoContext' verbosity remoteRepos [] cacheDir Nothing (Just True) [] $
            \RepoContext{..} ->
                for_ repoContextRepos $ \case
                    repo@RepoSecure{repoRemote} ->
                        repoContextWithSecureRepo repo $ \repoSecure -> do
                            let name = unRepoName (remoteRepoName repoRemote)
                            logger <& UpdatingRepository name (show (remoteRepoURI repoRemote))
                            updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure Nothing
                            logger <& RepositoryUpdated name (case updated of Sec.HasUpdates -> True; Sec.NoUpdates -> False)
                    _otherwise ->
                        pure ()

    for remoteRepos $ \RemoteRepo{..} -> do
        let path = cacheDir </> unRepoName remoteRepoName </> "01-index.tar"
        exists <- doesFileExist path
        unless exists
            $ fail
            $ "no package index for repository "
                ++ unRepoName remoteRepoName
                ++ " at "
                ++ path
                ++ if offline then " (run without --offline to download it)" else ""
        return
            Repository
                { repositoryName = unRepoName remoteRepoName
                , repositoryUri = remoteRepoURI
                , repositoryIndexTar = path
                }
  where
    mkRepo name uri = (emptyRemoteRepo (RepoName name)){remoteRepoURI = uri}
    remoteRepos =
        -- Mark all repositories as secure
        map (\r -> r{remoteRepoSecure = Just True})
            -- If the user has specified only a uri, we use a sequential number
            -- as repository name.
            $ zipWith
                (\num -> either (mkRepo ("repository" ++ show num)) id)
                [1 :: Int ..]
                repositories

-- | Read a local package: a directory with one .cabal file, or the .cabal
-- file itself.
readLocalPackage :: FilePath -> IO (SourcePackage PackageSource)
readLocalPackage path = do
    isDir <- doesDirectoryExist path
    (dir, cabalFile) <-
        if isDir
            then
                filter ((== ".cabal") . takeExtension) <$> listDirectory path >>= \case
                    [f] -> return (path, path </> f)
                    fs -> fail ("expected one .cabal file in " ++ path ++ ", found " ++ show (length fs))
            else return (takeDirectory path, path)
    content <- BS.readFile cabalFile
    gpd <- case parseGenericPackageDescriptionMaybe content of
        Just gpd -> return (gpd :: GenericPackageDescription)
        Nothing -> fail ("cannot parse " ++ cabalFile)
    return
        SourcePackage
            { srcpkgPackageId = package (packageDescription gpd)
            , srcpkgDescription = gpd
            , srcpkgSource = LocalPackage dir
            , srcpkgDescrOverride = Nothing
            }

-- | Read a packagedb. Cabal parses the output of ghc-pkg but we cannot
-- assume we have access to ghc-pkg, so we read the files from a given
-- directory. Note that the file format is still fixed by Cabal itself.
readPackageDb :: Logger -> FilePath -> IO InstalledPackageIndex
readPackageDb logger path = do
    entriesFilename <-
        filter ((== ".conf") . takeExtension)
            <$> listDirectory path
    Cabal.Index.fromList
        <$> for
            entriesFilename
            ( \filename -> do
                content <- BS.readFile (path </> filename)
                case parseInstalledPackageInfo content of
                    Left e -> fail (unlines $ ("cannot parse " ++ path </> filename ++ ":") : NE.toList e)
                    Right (warnings, ipi) -> do
                        for_ warnings ((logger <&) . FileWarning (path </> filename))
                        return (ipi :: InstalledPackageInfo)
            )
