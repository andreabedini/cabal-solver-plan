{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils (Index (..), TotalIndexState, getSourcePackagesAtIndexState, updateRepoIndexCache)
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types (RemoteRepo (..), Repo (..), RepoName (..), SourcePackageDb (..), emptyRemoteRepo)
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageName, compilerInfo)
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Solver.Modular (SolverConfig (..))
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.Configured (CP (..))
import Distribution.Solver.Modular.ConflictSet (showConflictSet)
import Distribution.Solver.Modular.Log (SolverFailure (BackjumpLimitReached, ExhaustiveSearch))
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.Package (showPI)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ComponentDeps qualified as ComponentDeps
import Distribution.Solver.Types.OptionalStanza (showStanzas)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..), readPkgConfigDb)
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.Solver.Types.Settings (PreferOldest (..))
import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (PackageVersionConstraint))
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Client qualified as Sec
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import Options
import Options.Applicative
import Solver (compute)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Prelude hiding (pi)

data Options = Options
  { compilerSource :: CompilerSource,
    remoteRepos :: [RemoteRepo],
    mTotalIndexState :: Maybe TotalIndexState,
    pkgConfigDbSources :: [PkgConfigDbSource],
    constraints :: [UserConstraint],
    preferences :: [PackageVersionConstraint],
    flagAssignments :: Map PackageName FlagAssignment,
    solverConfig :: SolverConfig,
    preferOldest :: PreferOldest,
    cacheDir :: FilePath,
    offline :: Bool,
    targets :: [PackageVersionConstraint]
  }

opts :: ParserInfo Options
opts = info (s <**> helper) fullDesc
  where
    s :: Parser Options
    s =
      Options
        <$> compilerSourceParser
        <*> many repositoryParser
        <*> optional (parsecOption (long "index-state"))
        <*> many pkgConfigDbSourceParser
        <*> many (parsecOption (long "constraint"))
        <*> pure [] -- TODO: solverSettingPreferences
        <*> flagAssignmentParser
        <*> solverConfigParser
        <*> (PreferOldest <$> switch (long "prefer-oldest"))
        <*> strOption (long "cache-dir" <> value "_cache")
        <*> flag False True (long "offline")
        <*> many (parsecArgument (metavar "TARGET"))

-- NOTE: all this it to allow users to use a shorter form for reposiotries
-- cabal requires repo-name:repo-uri but we try to make up a repo-name from
-- the uri.
-- Also not the clearest code.
repositoryParser :: Parser RemoteRepo
repositoryParser = option g (long "repository")
  where
    g =
      str >>= \s ->
        maybe
          (fail "cannot parse repository")
          (return . makeSecure)
          $ (parseAbsoluteURI s >>= guessRepoName) <|> simpleParsec s

    guessRepoName uri =
      case uri of
        URI {uriScheme, uriPath}
          | uriScheme == "file:" ->
              Just (emptyRemoteRepo (RepoName (takeBaseName uriPath))) {remoteRepoURI = uri}
        URI {uriScheme, uriPath, uriAuthority = Just (URIAuth {uriRegName})}
          | null uriPath,
            uriScheme `elem` ["http:", "https:"] ->
              Just (emptyRemoteRepo (RepoName uriRegName)) {remoteRepoURI = uri}
        _otherwise -> Nothing

    makeSecure repo = repo {remoteRepoSecure = Just True}

readPackageDb :: FilePath -> IO InstalledPackageIndex
readPackageDb path = do
  filenames <- listDirectory path
  let packageDbEntryFiles = filter ((== ".conf") . takeExtension) filenames
  ipis <- for packageDbEntryFiles $ \filename -> do
    content <- BS.readFile (path </> filename)
    case parseInstalledPackageInfo content of
      Left e -> fail (unlines $ NE.toList e)
      Right (warnings, ipi) -> do
        for_ warnings print
        return ipi
  return $ Cabal.Index.fromList ipis

main :: IO ()
main = do
  Options {..} <- execParser opts

  (cinfo, os, arch, installedPkgIndex) <-
    case compilerSource of
      CompilerInline cinfo os arch packagedbs -> do
        installedPkgIndex <- foldMap readPackageDb packagedbs
        return (cinfo, os, arch, installedPkgIndex)
      CompilerFromCompiler mHcPath packagedbs -> do
        (compiler, mPlatform, programdb) <- GHC.configure Verbosity.normal mHcPath Nothing defaultProgramDb
        let Platform arch os = fromMaybe (Platform buildArch buildOS) mPlatform
        installedPkgIndex <- GHC.getInstalledPackages Verbosity.normal compiler packagedbs programdb
        return (compilerInfo compiler, os, arch, installedPkgIndex)

  pkgConfigDb <-
    case NE.nonEmpty pkgConfigDbSources of
      Nothing -> return NoPkgConfigDb
      Just sources ->
        PkgConfigDb . Map.fromList
          <$> foldMap
            ( \case
                PkgConfigDbEntry name mVersion -> return [(name, mVersion)]
                PkgConfigDbSystem ->
                  readPkgConfigDb Verbosity.normal defaultProgramDb <&> \case
                    PkgConfigDb m -> Map.toList m
                    NoPkgConfigDb -> mempty
            )
            sources

  -- NOTE: This is workaround for a bug in cabal-install where it
  -- tries to create hackage-security-lock in repoLocalDir, without
  -- making sure that directory exists.
  for_ remoteRepos $ \RemoteRepo {remoteRepoName} ->
    createDirectoryIfMissing True (cacheDir </> unRepoName remoteRepoName)

  (SourcePackageDb sourcePkgIndex sourcePkgPrefs, _tis, _ar) <-
    withRepoContext' Verbosity.normal remoteRepos [] cacheDir Nothing (Just True) [] $
      \repoContext@RepoContext {..} -> do
        unless offline $
          for_ repoContextRepos $ \case
            repo@RepoSecure {repoRemote} ->
              repoContextWithSecureRepo repo $ \repoSecure -> do
                updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure Nothing
                case updated of
                  Sec.NoUpdates ->
                    putStrLn $ "no updates for " ++ prettyShow repoRemote
                  Sec.HasUpdates -> do
                    putStrLn $ "updates available for " ++ prettyShow repoRemote ++ " refreshing the cache ..."
                    updateRepoIndexCache Verbosity.normal (RepoIndex repoContext repo)
            _otherwise ->
              pure ()

        getSourcePackagesAtIndexState Verbosity.normal repoContext mTotalIndexState Nothing

  let targetsPackageName = Set.fromList [pn | PackageVersionConstraint pn _ <- targets]

      progress =
        toProgress $
          compute
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
            unwords [showPI pi, prettyShow flagAssignment, showStanzas optionalStanzaSet]
              : map
                (\g -> prettyShow (fst (NE.head g)) ++ "\t" ++ intercalate ", " (foldMap (map showPI . snd) g))
                (NE.groupWith fst $ ComponentDeps.toList componentDeps)
