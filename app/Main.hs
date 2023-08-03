{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Distribution.Client.GlobalFlags (RepoContext (..), withRepoContext')
import Distribution.Client.IndexUtils (Index (..), getSourcePackagesAtIndexState, updateRepoIndexCache)
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types (RemoteRepo (..), Repo (..), RepoName (..), SourcePackageDb (..), emptyRemoteRepo)
import Distribution.Compat.Graph (nodeKey)
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageName, compilerInfo)
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Utils (ordNubBy)
import Distribution.Solver.Modular (SolverConfig (..))
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..), readPkgConfigDb)
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.Solver.Types.Settings (PreferOldest (..))
import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint)
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Client qualified as Sec
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import Options
import Options.Applicative
import Solver (compute)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

data Options = Options
  { compilerSource :: CompilerSource,
    pkgConfigDbSources :: [PkgConfigDbSource],
    constraints :: [(UserConstraint, ConstraintSource)],
    preferences :: [PackageVersionConstraint],
    flagAssignments :: Map PackageName FlagAssignment,
    solverConfig :: SolverConfig,
    preferOldest :: PreferOldest,
    remoteRepos :: [RemoteRepo],
    targets :: Set PackageName,
    cacheDir :: FilePath
  }

opts :: ParserInfo Options
opts = info (s <**> helper) fullDesc
  where
    s :: Parser Options
    s =
      Options
        <$> compilerSourceParser
        <*> many pkgConfigDbSourceParser
        <*> many constraintsParser
        <*> pure [] -- TODO: solverSettingPreferences
        <*> flagAssignmentParser
        <*> solverConfigParser
        <*> (PreferOldest <$> switch (long "prefer-oldest"))
        <*> many repositoryParser
        <*> (Set.fromList <$> many (parsecArgument (metavar "TARGET")))
        <*> strOption (long "cache-dir" <> value "_cache")

-- NOTE: all this it to allow users to use a shorter form for reposiotries
-- cabal requires repo-name:repo-uri but we try to make up a repo-name from its
-- uri. Also not the clearest code.
repositoryParser :: Parser RemoteRepo
repositoryParser = option g (long "repository")
  where
    g =
      makeSecure
        <$> ( str
                >>= \s ->
                  maybe (fail "cannot parse repository") return ((parseAbsoluteURI s >>= f) <|> simpleParsec s)
            )

    f uri@(URI {uriScheme, uriPath})
      | uriScheme == "file:" =
          Just (emptyRemoteRepo (RepoName (takeBaseName uriPath))) {remoteRepoURI = uri}
    f uri@(URI {uriScheme, uriPath, uriAuthority = Just (URIAuth {uriRegName})})
      | null uriPath,
        uriScheme `elem` ["http:", "https:"] =
          Just (emptyRemoteRepo (RepoName uriRegName)) {remoteRepoURI = uri}
    f _otherwise = Nothing

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
    withRepoContext' Verbosity.normal remoteRepos [] cacheDir Nothing (Just True) [] $ \repoContext@RepoContext {..} -> do
      for_ repoContextRepos $ \case
        repo@RepoSecure {repoRemote} -> do
          repoContextWithSecureRepo repo $ \repoSecure -> do
            updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure Nothing
            case updated of
              Sec.NoUpdates -> putStrLn $ "no updates for " ++ prettyShow repoRemote
              Sec.HasUpdates -> updateRepoIndexCache Verbosity.normal (RepoIndex repoContext repo)
        _otherwise -> pure ()

      getSourcePackagesAtIndexState Verbosity.normal repoContext Nothing Nothing

  foldProgress
    -- step
    (\step rest -> putStrLn step >> rest)
    -- fail
    (const $ putStrLn "fail" {- FIXME -})
    -- done
    ( \(assignment, rdm) -> do
        let resolverPkgs =
              ordNubBy nodeKey $ map (convCP installedPkgIndex sourcePkgIndex) (toCPs assignment rdm)
        for_ resolverPkgs print
    )
    $ showMessages
    $ toProgress
    $ compute
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
      targets
