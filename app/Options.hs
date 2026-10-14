{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Functor law" -}

module Options (
    parseOptions,
    Options (..),
    CompilerSource (..),
)
where

import Colog.Core (Severity (..))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Version (showVersion)
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.Setup (defaultMaxBackjumps)
import Distribution.Client.Targets
import Distribution.Client.Types.Repo
import Distribution.Compat.CharParsing qualified as P
import Distribution.Compiler (CompilerId)
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
import Language.Haskell.Extension (Extension, Language)
import Network.URI (URI (..), parseAbsoluteURI)
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
    { extraPackageDbs :: [FilePath]
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
    , deprecations :: [String]
    -- ^ Advice for each deprecated option that was used.
    , compilerSource :: CompilerSource
    }

parseOptions :: IO Options
parseOptions = do
    result <- customExecParser parserPrefs opts
    case result of
        Right options -> return options
        Left err -> handleParseResult (Failure (parserFailure parserPrefs opts (ErrorMsg err) []))

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs

-- | The options, or what is wrong with their combination.
opts :: ParserInfo (Either String Options)
opts = info (s <**> helper <**> simpleVersioner ("cabal-solver-plan " ++ showVersion version)) mods
  where
    s :: Parser (Either String Options)
    s =
        (\co mkOptions -> mkOptions (compilerDeprecations co) <$> toCompilerSource co)
            <$> compilerOptionsParser
            <*> rest

    rest :: Parser ([String] -> CompilerSource -> Options)
    rest =
        Options
            <$> many (strOption (long "package-db" <> metavar "PATH" <> help "Load packagedb at PATH"))
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

-- | Where the compiler comes from: a real GHC, which is run to find its
-- id, platform and global package db, or only a description of one.
data CompilerSource
    = CompilerInline
        CompilerInfo
        Platform
    | CompilerFromSystem
        -- | The path or name of GHC
        FilePath
        -- | Whether to load the compiler's global package db
        Bool

-- | The compiler options as given. 'toCompilerSource' checks that they go
-- together.
data CompilerOptions = CompilerOptions
    { withCompiler :: Maybe FilePath
    , compilerDeprecations :: [String]
    , noGlobalPackageDb :: Bool
    , inlineCompilerId :: Maybe CompilerId
    , inlineAbiTag :: Maybe AbiTag
    , inlineCompat :: Maybe [CompilerId]
    , inlineLanguages :: Maybe [Language]
    , inlineExtensions :: Maybe [Extension]
    , inlinePlatform :: Maybe Platform
    }

toCompilerSource :: CompilerOptions -> Either String CompilerSource
toCompilerSource CompilerOptions{..} = case inlineCompilerId of
    Nothing
        | not (null describing) ->
            Left $
                intercalate ", " describing
                    ++ " describe the compiler given by --compiler-id. Give --compiler-id too,"
                    ++ " or leave them out to plan for the GHC that --with-compiler runs."
        | otherwise ->
            Right (CompilerFromSystem (fromMaybe "ghc" withCompiler) (not noGlobalPackageDb))
    Just compilerId
        | Just _ <- withCompiler ->
            Left $
                "--with-compiler and --compiler-id cannot be used together: --with-compiler runs"
                    ++ " a GHC to find its id, platform and global package db, while --compiler-id"
                    ++ " describes a compiler without running it."
        | noGlobalPackageDb ->
            Left $
                "--no-global-package-db goes with --with-compiler. With --compiler-id there is"
                    ++ " no global package db; give the installed packages with --package-db."
        | otherwise ->
            Right $
                CompilerInline
                    CompilerInfo
                        { compilerInfoId = compilerId
                        , compilerInfoAbiTag = fromMaybe NoAbiTag inlineAbiTag
                        , compilerInfoCompat = inlineCompat
                        , compilerInfoLanguages = inlineLanguages
                        , compilerInfoExtensions = inlineExtensions
                        , -- Wired-in unit ids are unknown; the solver then acts pessimistically.
                          compilerInfoWiredInUnitIds = Nothing
                        }
                    (fromMaybe buildPlatform inlinePlatform)
  where
    describing =
        [ flagName
        | (flagName, given) <-
            [ ("--platform", isJust inlinePlatform)
            , ("--abi-tag", isJust inlineAbiTag)
            , ("--compiler-id-compat", isJust inlineCompat)
            , ("--supported-language", isJust inlineLanguages)
            , ("--supported-extension", isJust inlineExtensions)
            ]
        , given
        ]

compilerOptionsParser :: Parser CompilerOptions
compilerOptionsParser =
    mkCompilerOptions
        <$> optional
            ( strOption
                ( short 'w'
                    <> long "with-compiler"
                    <> metavar "PATH"
                    <> help "Plan for this GHC, a path or a program on the PATH (default: ghc). It is run to find its version, platform and global package db."
                )
            )
        -- The old name of --with-compiler.
        <*> optional (strOption (long "with-ghc" <> metavar "PATH" <> internal))
        <*> switch
            ( long "no-global-package-db"
                <> help "Do not load the global package db of --with-compiler's GHC."
            )
        -- The global package db is loaded by default now.
        <*> switch (long "use-global-packagedb" <> internal)
        <*> optional
            ( parsecOption
                ( long "compiler-id"
                    <> metavar "COMPILER-ID"
                    <> help "Plan for this compiler, e.g. ghc-9.12.2, without running it. There are no installed packages unless given with --package-db."
                )
            )
        <*> optional
            ( parsecOption
                ( long "abi-tag"
                    <> metavar "ABI-TAG"
                    <> help "With --compiler-id: the tag that tells apart incompatible ABIs on the same platform (default: none)."
                )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "compiler-id-compat"
                        <> metavar "COMPILER-ID,..."
                        <> help "With --compiler-id: other compilers it is compatible with."
                    )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "supported-language"
                        <> metavar "LANGUAGE,..."
                        <> help "With --compiler-id: the language standards it supports (default: all)."
                    )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "supported-extension"
                        <> metavar "EXTENSION,..."
                        <> help "With --compiler-id: the extensions it supports (default: all)."
                    )
            )
        <*> optional
            ( parsecOption
                ( long "platform"
                    <> metavar "PLATFORM"
                    <> help ("With --compiler-id: the platform to plan for, e.g. aarch64-linux (default: " ++ Cabal.prettyShow buildPlatform ++ ").")
                )
            )
  where
    mkCompilerOptions withCompiler withGhc noGlobal useGlobal cid abi compat langs exts platform =
        CompilerOptions
            { withCompiler = withCompiler <|> withGhc
            , compilerDeprecations =
                ["--with-ghc is deprecated, use --with-compiler (-w)." | Just _ <- [withGhc]]
                    ++ ["--use-global-packagedb is deprecated: the global package db is loaded by default." | useGlobal]
            , noGlobalPackageDb = noGlobal
            , inlineCompilerId = cid
            , inlineAbiTag = abi
            , inlineCompat = compat
            , inlineLanguages = langs
            , inlineExtensions = exts
            , inlinePlatform = platform
            }

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
