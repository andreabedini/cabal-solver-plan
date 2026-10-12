{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils (TotalIndexState)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), lookupIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Types (
    RemoteRepo (..),
    Repo (..),
    RepoName (..),
    emptyRemoteRepo,
 )
import Distribution.InstalledPackageInfo (InstalledPackageInfo, installedUnitId, parseInstalledPackageInfo)
import Distribution.Pretty qualified as Cabal
import Distribution.Simple (CompilerInfo, PackageDBX (..), compilerInfo)
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.Configured (CP (..))
import Distribution.Solver.Modular.ConflictSet (showConflictSet)
import Distribution.Solver.Modular.Log (SolverFailure (..))
import Distribution.Solver.Modular.Message (renderSummarizedMessage, summarizeMessages)
import Distribution.Solver.Modular.Package (showPI)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.OptionalStanza (showStanzas)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..))
import Distribution.Solver.Types.PkgConfigDb qualified as Solver
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.System (Platform (..), buildPlatform)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Client qualified as Sec
import IndexTar (Index (..), Repository (..), indexSourcePackages, readIndex)
import Network.URI (URI (..), URIAuth (..))
import Options
import Plan (PlanInputs (..), makePlan, renderPlanJson)
import SetupDeps (applyDefaultSetupDeps)
import SolverInterface (solve)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStr, hPutStrLn, stderr)
import Prelude hiding (pi)

main :: IO ()
main = do
    Options{..} <- parseOptions

    -- Everything but the plan goes to stderr.
    let verbosity =
            Verbosity.setVerbosityHandles (Just stderr) $
                Verbosity.mkVerbosity Verbosity.defaultVerbosityHandles Verbosity.normal

    -- Here we interpret the user input w.r.t to the compiler, platform and
    -- package databases. While it is possible to specify these pieces
    -- idependently; they lose their independence if we let the user refer to
    -- an installed version of GHC. So there are two cases here: with or
    -- without access to a real compiler.
    (cinfo, platform@(Platform arch os), globalIPI) <-
        determineCompiler verbosity compilerSource

    extraIPIs <- for extraPackageDbs $ \path -> (,) path <$> readPackageDb path
    let installedPkgIndex = globalIPI <> foldMap snd extraIPIs
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
        fromRepositories <- loadRepositories verbosity repositories cacheDir offline
        let fromIndex =
                [ Repository
                    { repositoryName = maybe "index" uriRegName (uriAuthority indexUri)
                    , repositoryUri = indexUri
                    , repositoryIndexTar = path
                    }
                | Just path <- [indexTar]
                ]
        for (fromIndex ++ fromRepositories) $ \repo -> do
            index <- readIndex repo (indexStateOf repo mTotalIndexState)
            hPutStrLn stderr $
                "Index " ++ repositoryName repo ++ ": " ++ show (Map.size (indexPackages index)) ++ " package versions"
                    ++ ", index-state " ++ Cabal.prettyShow (indexTimestamp index)
            return index

    -- As cabal-install does, give Custom and Hooks packages without a
    -- custom-setup stanza default setup dependencies.
    sourcePkgIndex <-
        fmap (applyDefaultSetupDeps cinfo platform) . mconcat
            <$> traverse indexSourcePackages indexes
    let sourcePkgPrefs = Map.unionsWith (const id) (map indexPreferences indexes)

    let targetsPackageName =
            Set.fromList [pn | PackageVersionConstraint pn _ <- targets]

        progress =
            toProgress $
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
                    targetsPackageName

    result <-
        foldProgress
            -- step
            (\step rest -> hPutStrLn stderr (renderSummarizedMessage step) >> rest)
            -- fail
            (return . Left)
            -- done
            (return . Right . uncurry toCPs)
            (summarizeMessages progress)

    case result of
        Left (ExhaustiveSearch cs cm) -> do
            hPutStr stderr $
                unlines
                    [ "Exhaustive search failed"
                    , "conflict set: " ++ showConflictSet cs
                    , "conflict map: " ++ show cm
                    ]
            exitFailure
        Left BackjumpLimitReached -> do
            hPutStrLn stderr "backjump limit reached"
            exitFailure
        Right cps
            | jsonOutput -> do
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
                LBS.putStr $
                    renderPlanJson
                        [(repositoryName (indexRepository i), indexTimestamp i) | i <- indexes]
                        plan
            | otherwise ->
                for_ cps $ \(CP pi flagAssignment optionalStanzaSet componentDeps) ->
                    putStrLn $
                        unlines $
                            unwords
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
    Verbosity ->
    [Either URI RemoteRepo] ->
    FilePath ->
    Bool ->
    IO [Repository]
loadRepositories verbosity repositories cacheDir offline = do
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
                            hPutStr stderr $ "Updating " ++ Cabal.prettyShow repoRemote ++ " ..."
                            updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure Nothing
                            hPutStrLn stderr $ case updated of
                                Sec.NoUpdates -> " no updates found."
                                Sec.HasUpdates -> " updated."
                    _otherwise ->
                        pure ()

    for remoteRepos $ \RemoteRepo{..} -> do
        let path = cacheDir </> unRepoName remoteRepoName </> "01-index.tar"
        exists <- doesFileExist path
        unless exists $ do
            hPutStrLn stderr $
                "No package index for repository " ++ unRepoName remoteRepoName ++ " at " ++ path
                    ++ if offline then " (run without --offline to download it)" else ""
            exitFailure
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

-- | Read a packagedb. Cabal parses the output of ghc-pkg but we cannot
-- assume we have access to ghc-pkg, so we read the files from a given
-- directory. Note that the file format is still fixed by Cabal itself.
readPackageDb :: FilePath -> IO InstalledPackageIndex
readPackageDb path = do
    entriesFilename <-
        filter ((== ".conf") . takeExtension)
            <$> listDirectory path
    Cabal.Index.fromList
        <$> for
            entriesFilename
            ( \filename -> do
                content <- BS.readFile (path </> filename)
                case parseInstalledPackageInfo content of
                    Left e -> fail (unlines $ NE.toList e)
                    Right (warnings, ipi) -> do
                        for_ warnings (hPutStrLn stderr)
                        return (ipi :: InstalledPackageInfo)
            )
