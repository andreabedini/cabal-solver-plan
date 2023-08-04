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
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils
  ( Index (..),
    TotalIndexState,
    getSourcePackagesAtIndexState,
    updateRepoIndexCache,
  )
import Distribution.Client.Types
  ( RemoteRepo (..),
    Repo (..),
    RepoName (..),
    SourcePackageDb (SourcePackageDb),
    emptyRemoteRepo,
  )
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Pretty qualified as Cabal
import Distribution.Simple (CompilerInfo, compilerInfo)
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
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.System (Arch, OS, Platform (..), buildArch, buildOS)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
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
  Options {..} <- parseOptions

  -- Here we interpret the user input w.r.t to the compiler, platform and
  -- package databases. While it is possible to specify these pieces
  -- idependently; they lose their independence if we let the user refer to
  -- an installed version of GHC. So there are two cases here: with or
  -- without access to a real compiler.
  (cinfo, os, arch, installedPkgIndex) <-
    determineCompiler verbosity compilerSource

  pkgConfigDb <- readPkgConfigDb verbosity pkgConfigDbSources

  SourcePackageDb sourcePkgIndex sourcePkgPrefs <-
    loadRepositories
      verbosity
      repositories
      mTotalIndexState
      cacheDir
      offline

  let targetsPackageName =
        Set.fromList [pn | PackageVersionConstraint pn _ <- targets]

      progress =
        toProgress $
          solve
            cinfo
            os
            arch
            pkgConfigDb
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
      (\step rest -> putStrLn step >> rest)
      -- fail
      (return . Left)
      -- done
      (return . Right . uncurry toCPs)
      (showMessages progress)

  case result of
    Left (ExhaustiveSearch cs cm) ->
      putStrLn $
        unlines
          [ "Exhaustive search failed",
            "conflict set: " ++ showConflictSet cs,
            "conflict map: " ++ show cm
          ]
    Left BackjumpLimitReached ->
      putStrLn "backjump limit reached"
    Right cps ->
      for_ cps $ \(CP pi flagAssignment optionalStanzaSet componentDeps) ->
        putStrLn $
          unlines $
            unwords
              [ showPI pi,
                Cabal.prettyShow flagAssignment,
                showStanzas optionalStanzaSet
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
  IO (CompilerInfo, OS, Arch, InstalledPackageIndex)
determineCompiler verbosity = \case
  CompilerInline cinfo os arch packagedbs -> do
    installedPkgIndex <- foldMap readPackageDb packagedbs
    return (cinfo, os, arch, installedPkgIndex)
  CompilerFromSystem mHcPath packagedbs -> do
    (compiler, mPlatform, programdb) <-
      GHC.configure verbosity mHcPath Nothing defaultProgramDb
    let Platform arch os =
          fromMaybe (Platform buildArch buildOS) mPlatform
    installedPkgIndex <-
      GHC.getInstalledPackages verbosity compiler packagedbs programdb
    return (compilerInfo compiler, os, arch, installedPkgIndex)

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
  for_ remoteRepos $ \RemoteRepo {remoteRepoName} ->
    createDirectoryIfMissing True (cacheDir </> unRepoName remoteRepoName)

  (srcPkgDb, tis, ar) <-
    withRepoContext' verbosity remoteRepos [] cacheDir Nothing (Just True) [] $
      \repoContext@RepoContext {..} -> do
        for_ repoContextRepos $ \case
          repo@RepoSecure {repoRemote} -> do
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
    mkRepo name uri = (emptyRemoteRepo (RepoName name)) {remoteRepoURI = uri}
    remoteRepos =
      -- Mark all repositories as secure
      map (\r -> r {remoteRepoSecure = Just True})
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
  ipis <- for entriesFilename $ \filename -> do
    content <- BS.readFile (path </> filename)
    case parseInstalledPackageInfo content of
      Left e -> fail (unlines $ NE.toList e)
      Right (warnings, ipi) -> do
        for_ warnings print
        return ipi
  return $ Cabal.Index.fromList ipis

-- | Obtain a pkgconfig database. We either get the available packages from
-- the command line or we use the system pkg-config.
readPkgConfigDb :: Verbosity -> [PkgConfigDbSource] -> IO PkgConfigDb
readPkgConfigDb verbosity pkgConfigDbSources =
  case NE.nonEmpty pkgConfigDbSources of
    Nothing -> return NoPkgConfigDb
    Just sources ->
      PkgConfigDb . Map.fromList
        <$> foldMap
          ( \case
              PkgConfigDbEntry name mVersion ->
                return [(name, mVersion)]
              PkgConfigDbFromSystem ->
                Solver.readPkgConfigDb verbosity defaultProgramDb <&> \case
                  PkgConfigDb m -> Map.toList m
                  NoPkgConfigDb -> mempty
          )
          sources
