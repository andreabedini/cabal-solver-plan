{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Functor law" -}

module Options (
    parseOptions,
    Options (..),
    CompilerSource (..),
)
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.Setup (defaultMaxBackjumps)
import Distribution.Client.Targets
import Distribution.Client.Types.Repo
import Distribution.Compat.CharParsing qualified as P
import Distribution.PackageDescription (
    ComponentName (..),
    LibraryName (..),
    PackageId,
    PkgconfigVersion,
    parsecFlagAssignmentNonEmpty,
 )
import Distribution.Parsec (Parsec (parsec))
import Distribution.Parsec qualified as Cabal
import Distribution.Parsec qualified as Parsec
import Distribution.Pretty qualified as Cabal
import Distribution.Simple (AbiTag (..), CompilerInfo (..), PackageName, PkgconfigName)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Types.Settings (
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
import Distribution.System (Platform, buildPlatform)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint
import Distribution.Verbosity qualified as Verbosity
import Network.URI (URI (..), parseAbsoluteURI)
import Colog.Core (Severity (..))
import Options.Applicative
import Paths_cabal_solver_plan (version)

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
    { compilerSource :: CompilerSource
    , extraPackageDbs :: [FilePath]
    , extraPreInstalled :: [(PackageId, ComponentName)]
    , repositories :: [Either URI RemoteRepo]
    , indexTar :: Maybe FilePath
    , localPackages :: [FilePath]
    , indexUri :: URI
    , mTotalIndexState :: Maybe TotalIndexState
    , useSystemPkgConfigDb :: Bool
    , pkgConfigDbEntries :: [(PkgconfigName, Maybe PkgconfigVersion)]
    , constraints :: [UserConstraint]
    , preferences :: [PackageVersionConstraint]
    , flagAssignments :: Map PackageName FlagAssignment
    , solverConfig :: SolverConfig
    , preferOldest :: PreferOldest
    , cacheDir :: FilePath
    , offline :: Bool
    , jsonOutput :: Bool
    , logLevel :: Severity
    , targets :: [PackageVersionConstraint]
    }

parseOptions :: IO Options
parseOptions = execParser opts

opts :: ParserInfo Options
opts = info (s <**> helper <**> simpleVersioner ("cabal-solver-plan " ++ showVersion version)) mods
  where
    s :: Parser Options
    s =
        Options
            <$> compilerSourceParser
            <*> many (strOption (long "package-db" <> metavar "PATH" <> help "Load packagedb at PATH"))
            <*> many extraPreInstalledParser
            <*> many repositoryParser
            <*> optional
                ( strOption
                    ( long "index"
                        <> metavar "PATH"
                        <> help "Read source packages from the package index (01-index.tar) at PATH. Never uses the network."
                    )
                )
            <*> many
                ( strOption
                    ( long "local-package"
                        <> metavar "PATH"
                        <> help "A local package (as in cabal.project's packages:): a directory with one .cabal file, or the .cabal file. Local packages are always targets."
                    )
                )
            <*> option
                (maybeReader parseRepositoryUri)
                ( long "index-uri"
                    <> metavar "URI"
                    <> help "The repository the packages in --index are downloaded from."
                    <> value hackageUri
                    <> showDefault
                )
            <*> optional (parsecOption (long "index-state"))
            <*> switch (long "use-system-pkg-config" <> help "Use system pkg-config.")
            <*> many pkgConfEntryParser
            <*> many (parsecOption (long "constraint"))
            <*> pure [] -- TODO: solverSettingPreferences
            <*> flagAssignmentParser
            <*> solverConfigParser
            <*> (PreferOldest <$> switch (long "prefer-oldest"))
            <*> strOption (long "cache-dir" <> value "_cache")
            <*> flag False True (long "offline" <> help "Do not update repositories; fail if a repository has not been downloaded.")
            <*> switch (long "json" <> help "Print the plan as JSON (see docs/plan-format.md).")
            <*> logLevelParser
            <*> many (parsecArgument (metavar "TARGET"))

    mods =
        mconcat
            [ fullDesc
            , header "cabal-solver-plan - run cabal-install's solver on inputs given on the command line"
            , progDesc "Find a build plan for the TARGET packages. The solver trace goes to stderr, the plan to stdout."
            , footer "See docs/plan-format.md for the --json output."
            ]

extraPreInstalledParser :: Parser (PackageId, ComponentName)
extraPreInstalledParser = parsecOptionWith g (long "with-preinstalled")
  where
    g = do
        pkgId <- Parsec.parsec
        libName <- optional (P.string ":" *> Parsec.parsec)
        return (pkgId, fromMaybe (CLibName LMainLibName) libName)

-- | A repository is given as NAME:URI, as in cabal, or as just a URI, in
-- which case we make up a name. Since NAME:URI is also an absolute URI
-- (with scheme NAME), we take it as a plain URI only if it looks like one.
repositoryParser :: Parser (Either URI RemoteRepo)
repositoryParser =
    option
        (eitherReader parseRepository)
        ( long "repository"
            <> metavar "[NAME:]URI"
            <> help "Download source packages from the (secure) repository at URI."
        )
  where
    parseRepository s
        | Just uri <- parseRepositoryUri s
        , uriScheme uri `elem` ["http:", "https:", "file:"] =
            Right (Left uri)
        | otherwise = do
            repo <- Parsec.eitherParsec s
            return (Right repo{remoteRepoURI = withTrailingSlash (remoteRepoURI repo)})

-- | Parse a repository URI. Its path always ends in a slash, since the
-- files of the repository are found relative to it.
parseRepositoryUri :: String -> Maybe URI
parseRepositoryUri = fmap withTrailingSlash . parseAbsoluteURI

withTrailingSlash :: URI -> URI
withTrailingSlash uri
    | null (uriPath uri) || last (uriPath uri) /= '/' = uri{uriPath = uriPath uri ++ "/"}
    | otherwise = uri

hackageUri :: URI
hackageUri = fromMaybe (error "hackageUri") (parseRepositoryUri "https://hackage.haskell.org/")

data CompilerSource
    = CompilerInline
        CompilerInfo
        Platform
    | CompilerFromSystem
        -- | sdfsd
        FilePath
        -- | Whether or not to load the compiler's global packagedb
        Bool

compilerSourceParser :: Parser CompilerSource
compilerSourceParser =
    compilerFromSystem <|> compilerInline
  where
    compilerFromSystem =
        CompilerFromSystem
            <$> strOption
                ( long "with-ghc"
                    <> help "Use an installed compiler available at PATH."
                    <> metavar "PATH"
                    <> value "ghc"
                    <> showDefault
                )
            <*> switch
                ( long "use-global-packagedb"
                    <> help "Use the compiler's global package db"
                )

    compilerInline =
        CompilerInline
            <$> compilerInfoOption
            <*> platformOption

    compilerInfoOption =
        CompilerInfo
            <$> parsecOption
                ( long "compiler-id"
                    <> help "Compiler flavour and version. E.g \"ghc-9.4.7\"."
                    <> metavar "COMPILER-ID"
                )
            <*> parsecOption
                ( long "abi-tag"
                    <> help "Tag for distinguishing incompatible ABI's on the same architecture/os."
                    <> metavar "ABI-TAG"
                    <> value NoAbiTag
                    <> showDefaultWith (const "no tag")
                )
            <*> optional
                ( unCommaSeparated
                    <$> parsecOption
                        ( long "compiler-id-compat"
                            <> help "Other implementations that this compiler claims to be compatible with, if known."
                            <> metavar "COMPILER-ID"
                        )
                )
            <*> optional
                ( unCommaSeparated
                    <$> parsecOption
                        ( long "supported-language"
                            <> help "Supported language standards, if known. If no language standard is specified the solver assumes any standard is supported."
                            <> metavar "LANGUAGE"
                        )
                )
            <*> optional
                ( unCommaSeparated
                    <$> parsecOption
                        ( long "supported-extension"
                            <> help "Supported extensions, if known. If no extension is specified the solver assumes any extension is supported."
                            <> metavar "EXTENSION"
                        )
                )
            -- Wired-in unit ids are unknown; the solver then acts pessimistically.
            <*> pure Nothing

    platformOption =
        parsecOption
            ( long "platform"
                <> help "The desired platform for the build plan."
                <> metavar "PLATFORM"
                <> value buildPlatform
                <> showDefaultWith Cabal.prettyShow
            )

pkgConfEntryParser :: Parser (PkgconfigName, Maybe PkgconfigVersion)
pkgConfEntryParser =
    parsecOptionWith
        entryParser
        ( long "with-pkg-config-module"
            <> help "Assume existence of pkg-config module NAME and VERSION"
            <> metavar "NAME[=VERSION]"
        )
  where
    entryParser = do
        pkgName <- Parsec.parsec
        pkgVersion <- optional (P.string "=" *> Parsec.parsec)
        return (pkgName, pkgVersion)

solverConfigParser :: Parser SolverConfig
solverConfigParser =
    SolverConfig
        <$> ( ReorderGoals
                <$> option
                    auto
                    ( long "reorder-goals"
                        <> help "Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
                        <> value False
                        <> showDefault
                    )
            )
        <*> ( CountConflicts
                <$> option
                    auto
                    ( long "no-count-conflicts"
                        <> help "Try to speed up solving by preferring goals that are involved in a lot of conflicts."
                        <> value True
                        <> showDefault
                    )
            )
        <*> ( FineGrainedConflicts
                <$> option
                    auto
                    ( long "no-fine-grained-conflicts"
                        <> help "Skip a version of a package if it does not resolve the conflicts encountered in the last version, as a solver optimization."
                        <> value True
                        <> showDefault
                    )
            )
        <*> ( MinimizeConflictSet
                <$> option
                    auto
                    ( long "minimize-conflict-set"
                        <> help
                            ( "When there is no solution, try to improve the error message by finding "
                                ++ "a minimal conflict set (default: false). May increase run time "
                                ++ "significantly."
                            )
                        <> value False
                        <> showDefault
                    )
            )
        <*> ( IndependentGoals
                <$> option
                    auto
                    ( long "independent-goals"
                        <> help "Treat several goals on the command line as independent. If several goals depend on the same package, different versions can be chosen."
                        <> value False
                        <> showDefault
                    )
            )
        <*> ( AvoidReinstalls
                <$> option
                    auto
                    ( long "avoid-reinstalls"
                        <> help "Do not select versions that would destructively overwrite installed packages."
                        <> value False
                        <> showDefault
                    )
            )
        <*> ( ShadowPkgs
                <$> option
                    auto
                    ( long "shadow-pkgs"
                        <> help "If multiple package instances of the same version are installed, treat all but one as shadowed."
                        <> value False
                        <> showDefault
                    )
            )
        <*> ( StrongFlags
                <$> option
                    auto
                    ( long "strong-flags"
                        <> help "Do not defer flag choices (this used to be the default in cabal-install <= 1.20)."
                        <> value False
                        <> showDefault
                    )
            )
        <*> flag
            OnlyConstrainedNone
            OnlyConstrainedAll
            ( long "reject-unconstrained-dependencies"
                <> help "Require all packages to have constraints on them if they are to be selected."
            )
        <*> ( (\case n | n < 0 -> Nothing; n -> Just n)
                <$> option
                    auto
                    ( long "max-backjumps"
                        <> help "Maximum number of backjumps allowed while solving. Use a negative number to enable unlimited backtracking. Use 0 to disable backtracking completely."
                        <> value defaultMaxBackjumps
                        <> showDefault
                    )
            )
        <*> ( EnableBackjumping
                <$> option
                    auto
                    ( long "enable-backjumping"
                        <> value True
                        <> showDefault
                        -- I am not sure this makes sense so I'll hide this option
                        <> internal
                    )
            )
        <*> ( SolveExecutables
                <$> option
                    auto
                    ( long "solve-executables"
                        <> help "Whether or not to solve for dependencies on executables."
                        <> value True
                        <> showDefault
                    )
            )
        -- Skip goalOrder. It is a function
        <*> pure Nothing
        -- At verbose, the explanation of a failure would repeat the whole
        -- trace, which --verbose prints already.
        <*> pure (Verbosity.vLevel Verbosity.normal)
        <*> ( PruneAfterFirstSuccess
                <$> switch
                    ( long "prune-after-first-success"
                    )
            )

-- | The least severe messages to print: -v adds the solver's trace, -q
-- leaves out what is printed while working.
logLevelParser :: Parser Severity
logLevelParser =
    flag' Debug (short 'v' <> long "verbose" <> help "Also print the solver's trace.")
        <|> flag' Warning (short 'q' <> long "quiet" <> help "Print only warnings and errors.")
        <|> pure Info

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

newtype CommaSeparated a = CommaSeparated {unCommaSeparated :: [a]}

instance (Cabal.Parsec a) => Cabal.Parsec (CommaSeparated a) where
    parsec = CommaSeparated <$> (Cabal.parsec `P.sepBy` P.char ',')
