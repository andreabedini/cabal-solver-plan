{-# LANGUAGE LambdaCase #-}

module Options
  ( parseOptions,
    Options (..),
    CompilerSource (..),
    PkgConfigDbSource (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.Targets
import Distribution.Client.Types.Repo
import Distribution.Compat.CharParsing qualified as P
import Distribution.PackageDescription
  ( ComponentName (..),
    LibraryName (..),
    PackageId,
    PkgconfigVersion,
    parsecFlagAssignmentNonEmpty,
  )
import Distribution.Parsec qualified as Parsec
import Distribution.Simple (AbiTag (..), CompilerInfo (..), PackageDB (..), PackageDBStack, PackageName, PkgconfigName)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Types.Settings
  ( AllowBootLibInstalls (..),
    AvoidReinstalls (..),
    CountConflicts (..),
    EnableBackjumping (..),
    FineGrainedConflicts (..),
    IndependentGoals (..),
    MinimizeConflictSet (..),
    OnlyConstrained (..),
    PreferOldest (..),
    ReorderGoals (..),
    ShadowPkgs (..),
    SolveExecutables (..),
    StrongFlags (..),
  )
import Distribution.System (Arch, OS, buildArch, buildOS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint
import Distribution.Verbosity qualified as Verbosity
import Network.URI (URI (..), parseAbsoluteURI)
import Options.Applicative
import Text.Read (readMaybe)

-- | These are the options of the command line tool. They roughlty
-- correspond to the inputs required by the solver but there are some
-- deviations. There are few reasons to do this:
--
-- - Some inputs are impossible to express on the command line. E.g. the
-- list of source packages or pkgconfig entries. In this case I added
-- options to fetch these inputs in different forms (more or less pure).
--
-- - Some inputs are not independent. E.g. the global installed package
-- database should be provided by GHC (if it is available). In this case
-- I provide alternative set of options so that the user can only provide
-- sensible combinations (e.g. you cannot say --packagedb=global if GHC
-- is not installed).
--
-- - On last reason is simply convenience. E.g. the solver targets are
-- package names. So to find a plan for e.g. aeson-2.2.0.0, one would have
-- to give "aeson" as a target and "aeson == 2.2.0.0" as a constraint. I
-- adapted the command line options so that the user can specify directly
-- aeson-2.2.0.0 as target.
data Options = Options
  { compilerSource :: CompilerSource,
    extraPreInstalled :: [(PackageId, ComponentName)],
    repositories :: [Either URI RemoteRepo],
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

parseOptions :: IO Options
parseOptions = execParser opts

opts :: ParserInfo Options
opts = info (s <**> helper) fullDesc
  where
    s :: Parser Options
    s =
      Options
        <$> compilerSourceParser
        <*> many extraPreInstalledParser
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

extraPreInstalledParser :: Parser (PackageId, ComponentName)
extraPreInstalledParser = parsecOptionWith g (long "with-preinstalled")
  where
    g = do
      pkgId <- Parsec.parsec
      libName <- optional (P.string ":" *> Parsec.parsec)
      return (pkgId, fromMaybe (CLibName LMainLibName) libName)

-- NOTE: all this it to allow users to use a shorter form for reposiotries
-- cabal requires repo-name:repo-uri but we try to make up a repo-name
-- from the uri. Also not the clearest code.
repositoryParser :: Parser (Either URI RemoteRepo)
repositoryParser = option g (long "repository")
  where
    g =
      asum
        [ Left <$> maybeReader parseAbsoluteURI,
          Right <$> eitherReader Parsec.eitherParsec
        ]

data CompilerSource
  = CompilerInline CompilerInfo OS Arch [FilePath]
  | CompilerFromSystem (Maybe FilePath) PackageDBStack

compilerSourceParser :: Parser CompilerSource
compilerSourceParser =
  asum
    [ CompilerFromSystem
        <$> optArg "with-ghc" "GHC" str
        <*> many (option (parsePackageDb <$> str) (long "package-db" <> metavar "GLOBAL|USER|PATH")),
      CompilerInline
        <$> compilerInfoParser
        <*> osParser
        <*> archParser
        <*> many (strOption (long "package-db-dir" <> metavar "PATH"))
    ]
  where
    compilerInfoParser =
      CompilerInfo
        <$> parsecOption (long "compiler-id")
        <*> parsecOption (long "abi-tag" <> value NoAbiTag)
        <*> optional (some (parsecOption (long "compiler-id-compat")))
        <*> optional (some (parsecOption (long "language")))
        <*> optional (some (parsecOption (long "extension")))
    osParser = parsecOption (long "os" <> metavar "OS" <> value buildOS)
    archParser = parsecOption (long "arch" <> metavar "ARCH" <> value buildArch)

    parsePackageDb :: String -> PackageDB
    parsePackageDb "global" = GlobalPackageDB
    parsePackageDb "user" = UserPackageDB
    parsePackageDb other = SpecificPackageDB other

data PkgConfigDbSource
  = PkgConfigDbEntry PkgconfigName (Maybe PkgconfigVersion)
  | PkgConfigDbFromSystem

pkgConfigDbSourceParser :: Parser PkgConfigDbSource
pkgConfigDbSourceParser =
  asum
    [ parsecOptionWith entryParser (long "pkgconfig-entry"),
      flag' PkgConfigDbFromSystem (long "use-system-pkgconfig")
    ]
  where
    entryParser = do
      pkgName <- Parsec.parsec
      pkgVersion <- optional (P.string "=" *> Parsec.parsec)
      return $ PkgConfigDbEntry pkgName pkgVersion

solverConfigParser :: Parser SolverConfig
solverConfigParser =
  SolverConfig
    <$> (ReorderGoals <$> switch (long "reorder-goals"))
    <*> (CountConflicts <$> switch (long "count-conflicts"))
    <*> (FineGrainedConflicts <$> switch (long "fine-grained-conflicts"))
    <*> (MinimizeConflictSet <$> switch (long "minimize-conflict-set"))
    <*> (IndependentGoals <$> switch (long "independent-goals"))
    <*> (AvoidReinstalls <$> switch (long "avoid-reinstalls"))
    <*> (ShadowPkgs <$> switch (long "shadow-pkgs"))
    <*> (StrongFlags <$> switch (long "strong-flags"))
    <*> (AllowBootLibInstalls <$> switch (long "allow-boot-lib-installs"))
    <*> onlyConstrainedParser
    <*> optional (option (maybeReader readMaybe) (long "max-backjumps"))
    <*> (EnableBackjumping <$> switch (long "enable-backjumping"))
    <*> (SolveExecutables <$> switch (long "solve-executables"))
    <*> pure Nothing -- goalOrder
    <*> parsecOption (long "verbosity" <> value Verbosity.normal)
    <*> (PruneAfterFirstSuccess <$> switch (long "prune-after-first-success"))

onlyConstrainedParser :: Parser OnlyConstrained
onlyConstrainedParser =
  option reader (long "only-constrainted" <> value OnlyConstrainedNone)
  where
    reader =
      str >>= \case
        "none" -> pure OnlyConstrainedNone
        "all" -> pure OnlyConstrainedAll
        _ -> fail "bad only-constrainted"

flagAssignmentParser :: Parser (Map PackageName FlagAssignment)
flagAssignmentParser =
  Map.fromListWith (<>) <$> many (parsecOptionWith parser (long "flag"))
  where
    parser = (,) <$> Parsec.parsec <* P.char ':' <*> parsecFlagAssignmentNonEmpty

parsecOption :: (Parsec.Parsec a) => Mod OptionFields a -> Parser a
parsecOption = option (eitherReader Parsec.eitherParsec)

parsecOptionWith :: Parsec.ParsecParser a -> Mod OptionFields a -> Parser a
parsecOptionWith parser = option (eitherReader (Parsec.explicitEitherParsec parser))

parsecArgument :: (Parsec.Parsec a) => Mod ArgumentFields a -> Parser a
parsecArgument = argument (eitherReader Parsec.eitherParsec)

-- https://github.com/pcapriotti/optparse-applicative/issues/243#issuecomment-653924318
optArg :: String -> String -> ReadM a -> Parser (Maybe a)
optArg x meta rdr =
  flag' Nothing (long x <> style (<> fromString ("[=" <> meta <> "]")))
    <|> option (Just <$> rdr) (long x <> internal)
