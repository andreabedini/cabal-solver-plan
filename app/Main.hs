{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import Distribution.Client.Dependency.Types (Solver (..))
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils (
    Index (..),
    TotalIndexState,
    getSourcePackagesAtIndexState,
    updateRepoIndexCache,
 )
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning (planPackages)
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan (SolverInstallPlan))
import Distribution.Client.Types (
    RemoteRepo (..),
    Repo (..),
    RepoName (..),
    SourcePackageDb (SourcePackageDb),
    emptyRemoteRepo,
 )
import Distribution.Fields
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Pretty qualified as Cabal
import Distribution.Simple (Compiler, CompilerInfo, PackageDB (..), compilerInfo)
import Distribution.Simple.Flag
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.Configured (CP (..))
import Distribution.Solver.Modular.ConflictSet (showConflictSet)
import Distribution.Solver.Modular.Log (SolverFailure (..))
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.Package (showPI)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.OptionalStanza (showStanzas)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..))
import Distribution.Solver.Types.PkgConfigDb qualified as Solver
import Distribution.Solver.Types.Progress (Progress, foldProgress)
import Distribution.System (Platform (..), buildPlatform)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Utils.LogProgress
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Client qualified as Sec
import Network.URI (URI)
import Options
import SolverInterface (solve)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, (</>))
import Prelude hiding (pi)

main :: IO ()
main = do
    let verbosity = Verbosity.normal
    Options{..} <- parseOptions

    -- Here we interpret the user input w.r.t to the compiler, platform and
    -- package databases. While it is possible to specify these pieces
    -- idependently; they lose their independence if we let the user refer to
    -- an installed version of GHC. So there are two cases here: with or
    -- without access to a real compiler.
    (cinfo, platform@(Platform arch os), globalIPI) <-
        determineCompiler verbosity compilerSource

    extraIPIs <- foldMap readPackageDb extraPackageDbs
    let installedPkgIndex = globalIPI <> extraIPIs

    pkgConfigDb <- do
        systemPkgConfigEntries <-
            if useSystemPkgConfigDb
                then
                    Solver.readPkgConfigDb verbosity defaultProgramDb <&> \case
                        PkgConfigDb m ->
                            Map.toList m
                        NoPkgConfigDb ->
                            mempty
                else mempty

        pure $ PkgConfigDb $ Map.fromList $ systemPkgConfigEntries <> pkgConfigDbEntries

    sourcePkgDb <-
        loadRepositories
            verbosity
            repositories
            mTotalIndexState
            cacheDir
            offline

    -- this is needed if cabal.project has `import: https://`
    let fakeHttpTransport = undefined
        fakeDistDirLayout = undefined
    pcs <-
        -- This is real IO, TODO: try to keep it to bare minimum, e.g. read a single file
        -- OR we get rid of it and we create a ProjectConfig by hand
        runRebuild "" $
            readProjectConfig
                verbosity
                fakeHttpTransport
                NoFlag -- projectConfigIgnoreProject
                NoFlag -- projectConfigConfigFile
                fakeDistDirLayout

    let projectConfig =
            instantiateProjectConfigSkeletonWithCompiler
                os
                arch
                cinfo
                mempty -- FlagAssignment?
                pcs

    let solverSettings = resolveSolverSettings projectConfig

    let compiler :: Compiler
        compiler = _someconversion cinfo -- TODO: fill empty strings as compiler flags

    -- These are all environment things that elaborateInstallPlan does not really need to know
    let fakeCabalStoreDirLayout = undefined
        fakeInstallDirs = undefined

    let localPackages = []

    let
        solverPlan =
            foldProgress (const id) error id $
                planPackages
                    verbosity
                    compiler
                    platform
                    Modular
                    solverSettings
                    installedPkgIndex
                    sourcePkgDb
                    pkgConfigDb
                    localPackages
                    mempty -- localPackagesEnabledStanzas
    let (elaboratedPlan, elaboratedShared) =
            foldLogProgress $
                elaborateInstallPlan
                    verbosity
                    platform
                    compiler
                    defaultProgramDb -- ??
                    pkgConfigDb
                    fakeDistDirLayout
                    fakeCabalStoreDirLayout
                    solverPlan
                    localPackages
                    mempty -- sourcePackageHashes
                    fakeInstallDirs
                    (projectConfigShared projectConfig)
                    (projectConfigAllPackages projectConfig)
                    (projectConfigLocalPackages projectConfig)
                    (getMapMappend (projectConfigSpecificPackage projectConfig))

    let instantiatedPlan =
            instantiateInstallPlan
                fakeCabalStoreDirLayout
                fakeInstallDirs
                elaboratedShared
                elaboratedPlan

    putStrLn $ showInstallPlan instantiatedPlan

foldLogProgress :: LogProgress a -> a
foldLogProgress = _

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
                then GHC.getInstalledPackages verbosity compiler [GlobalPackageDB] programdb
                else mempty
        return (compilerInfo compiler, platform, installedPkgIndex)

-- | Load source package repositories.
--
-- Oh dear! This is a bit complicated but we can get it to work.
--
-- The user can specify a repository URI on the command line. We use
-- hackage-secuirity and a couple of cabal-install functions to fetch the
-- repository in a local cache directory. Than we read its content into a
-- package index.
--
-- Only secure repositories are supported. Note that secure repositories
-- do not have to be remote but can also be local, use a URI with file:
-- scheme.
--
-- If a cache repository is present it will be updated unless the --offline
-- option is passed.
loadRepositories ::
    Verbosity ->
    [Either URI RemoteRepo] ->
    Maybe TotalIndexState ->
    FilePath ->
    Bool ->
    IO SourcePackageDb
loadRepositories _verbosity repositories _mTotalIndexState _cacheDir _offline
    -- Ugly but we want to avoid calling withRepoContext' if we have no repository.
    | null repositories = return $ SourcePackageDb mempty mempty
loadRepositories verbosity repositories mTotalIndexState cacheDir offline = do
    -- Note: this is workaround for a bug in cabal-install where it
    -- tries to create hackage-security-lock in repoLocalDir, without
    -- making sure that directory exists.
    for_ remoteRepos $ \RemoteRepo{remoteRepoName} ->
        createDirectoryIfMissing True (cacheDir </> unRepoName remoteRepoName)

    (srcPkgDb, tis, ar) <-
        withRepoContext' verbosity remoteRepos [] cacheDir Nothing (Just True) [] $
            \repoContext@RepoContext{..} -> do
                for_ repoContextRepos $ \case
                    repo@RepoSecure{repoRemote} -> do
                        putStr $ "Loading " ++ Cabal.prettyShow repoRemote
                        unless offline $
                            repoContextWithSecureRepo repo $ \repoSecure -> do
                                putStr ", checking for updates ..."
                                updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure Nothing
                                case updated of
                                    Sec.NoUpdates ->
                                        putStrLn " no updates found."
                                    Sec.HasUpdates -> do
                                        putStrLn " updates available, refreshing the cache ..."
                                        updateRepoIndexCache verbosity (RepoIndex repoContext repo)
                                        putStrLn " done"
                    _otherwise ->
                        pure ()

                getSourcePackagesAtIndexState verbosity repoContext mTotalIndexState Nothing

    putStrLn $ "Active repositories: " ++ Cabal.prettyShow ar
    putStrLn $ "Total index-state: " ++ Cabal.prettyShow tis

    return srcPkgDb
  where
    mkRepo name uri = (emptyRemoteRepo (RepoName name)){remoteRepoURI = uri}
    remoteRepos =
        -- Mark all repositories as secure
        map (\r -> r{remoteRepoSecure = Just True})
        -- If the user has specified only a uri, we use a sequential number
        -- as repository name.
        $
            zipWith
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
                        for_ warnings print
                        return ipi
            )

-- TODO: see if we can read a project config without httpTransport
-- using parseLegacyProjectConfig and convertLegacyProjectConfig
--
-- all this amounts to non supporting ifs and imports
--
-- convertLegacyProjectConfig :: LegacyProjectConfig -> ProjectConfig
