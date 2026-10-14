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
import Data.List.NonEmpty qualified as NE
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
import Options.Applicative.Help (Chunk (..), isEmpty, paragraph, stringChunk)
import Options.Applicative.Help.Pretty (Doc, indent, pretty, vsep)
import Paths_cabal_solver_plan (version)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))

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
    , cacheDir :: Maybe FilePath
    , offline :: Bool
    , jsonOutput :: Bool
    , logLevel :: Severity
    , targets :: [PackageVersionConstraint]
    , deprecations :: [String]
    -- ^ Advice for each deprecated option that was used.
    , compilerSource :: CompilerSource
    }

-- | Parse the command line. Usage errors exit with 2.
parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    handleParseResult . usageErrors $ case execParserPure parserPrefs opts args of
        Success (Left err) -> Failure (usageError err)
        Success (Right options)
            | null (targets options) && null (localPackages options) -> Failure noTarget
            | otherwise -> Success options
        Failure failure -> Failure failure
        CompletionInvoked completion -> CompletionInvoked completion
  where
    usageError err = parserFailure parserPrefs opts (ErrorMsg err) []
    -- Most likely the program was run without arguments: say what it does.
    noTarget =
        overHelp
            ( \h ->
                h
                    { helpDescription = paragraph description
                    , helpFooter = Chunk (Just (vsep [examples, mempty, pretty seeHelp]))
                    }
            )
            (usageError "No TARGET given: name the packages to plan for, or give --local-package.")

-- | Usage errors exit with 2, and point to --help.
usageErrors :: ParserResult a -> ParserResult a
usageErrors = \case
    Failure (ParserFailure failure) -> Failure $ ParserFailure $ \progName ->
        case failure progName of
            (h, ExitFailure _, cols) ->
                ( h{helpFooter = if isEmpty (helpFooter h) then stringChunk seeHelp else helpFooter h}
                , ExitFailure 2
                , cols
                )
            success -> success
    other -> other

overHelp :: (ParserHelp -> ParserHelp) -> ParserFailure ParserHelp -> ParserFailure ParserHelp
overHelp f (ParserFailure failure) = ParserFailure $ \progName ->
    let (h, code, cols) = failure progName in (f h, code, cols)

seeHelp :: String
seeHelp = "Run 'cabal-solver-plan --help' for all options."

description :: String
description = "Find a build plan for the TARGET packages with cabal-install's solver, and print it on stdout."

examples :: Doc
examples =
    vsep
        [ pretty "Examples:"
        , indent 2 $
            vsep
                [ example
                    "Plan mtl, for the ghc on the PATH and the packages installed with it:"
                    "cabal-solver-plan mtl"
                , example
                    "Download (or update) Hackage's index, and print a plan for aeson as JSON:"
                    "cabal-solver-plan --repository hackage.haskell.org:https://hackage.haskell.org/ --json aeson"
                , example
                    "Plan for a GHC that is not installed, from an index file:"
                    "cabal-solver-plan --compiler-id ghc-9.12.2 --platform aarch64-linux --package-db DIR --index 01-index.tar text"
                ]
        ]
  where
    example what cmd = vsep [pretty (what :: String), indent 2 (pretty (cmd :: String))]

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs

-- | The options, or what is wrong with their combination.
opts :: ParserInfo (Either String Options)
opts = info (s <**> helper <**> simpleVersioner ("cabal-solver-plan " ++ showVersion version)) mods
  where
    s :: Parser (Either String Options)
    s =
        (\co mkOptions -> mkOptions (compilerDeprecations co) <$> toCompilerSource co)
            <$> parserOptionGroup "Compiler:" compilerOptionsParser
            <*> rest

    rest :: Parser ([String] -> CompilerSource -> Options)
    rest =
        Options
            <$> packages
                ( many
                    ( strOption
                        ( long "package-db"
                            <> metavar "DIR"
                            <> help "Also use the packages installed in DIR, a package db: a directory of .conf files, as made by ghc-pkg."
                            <> hidden
                        )
                    )
                )
            <*> packages (many extraPreInstalledParser)
            <*> packages (many repositoryParser)
            <*> packages
                ( optional
                    ( strOption
                        ( long "index"
                            <> metavar "PATH"
                            <> help "Use the packages of the package index (01-index.tar) at PATH, as it is, without the network."
                        )
                    )
                )
            <*> packages
                ( many
                    ( strOption
                        ( long "local-package"
                            <> metavar "PATH"
                            <> help "A local package (as in cabal.project's packages:): a directory with one .cabal file, or the .cabal file. Local packages are always targets."
                            <> hidden
                        )
                    )
                )
            <*> packages
                ( option
                    (maybeReader parseRepositoryUri)
                    ( long "index-uri"
                        <> metavar "URI"
                        <> help "With --index: the repository its packages are downloaded from."
                        <> value hackageUri
                        <> showDefault
                        <> hidden
                    )
                )
            <*> packages
                ( optional
                    ( parsecOption
                        ( long "index-state"
                            <> metavar "STATE"
                            <> help "Leave out the packages and revisions added to the indexes after STATE, a time such as 2026-06-01T00:00:00Z, or HEAD. As in cabal.project, a repository can have its own: 'hackage.haskell.org 2026-06-01T00:00:00Z, HEAD'."
                            <> hidden
                        )
                    )
                )
            <*> packages (switch (long "use-system-pkg-config" <> help "Use the pkg-config modules installed on this system." <> hidden))
            <*> packages (many pkgConfEntryParser)
            <*> solver
                ( many
                    ( parsecOption
                        ( long "constraint"
                            <> metavar "CONSTRAINT"
                            <> help "Constrain a package, as cabal's --constraint, e.g. 'aeson >= 2.2' or 'text -simdutf'."
                            <> hidden
                        )
                    )
                )
            <*> pure [] -- TODO: solverSettingPreferences
            <*> solver flagAssignmentParser
            <*> solver solverConfigParser
            <*> solver (PreferOldest <$> switchPair "prefer-oldest" False "Prefer the oldest versions of packages rather than the latest, e.g. to test lower bounds.")
            <*> packages
                ( optional
                    ( strOption
                        ( long "cache-dir"
                            <> metavar "DIR"
                            <> help "Where --repository keeps the repositories it downloads (default: cabal-solver-plan in the user's cache directory: $XDG_CACHE_HOME, ~/.cache, or %LOCALAPPDATA% on Windows)."
                            <> hidden
                        )
                    )
                )
            <*> packages (switch (long "offline" <> help "Use the repositories of --repository as downloaded before, without updating them." <> hidden))
            <*> output (switch (long "json" <> help "Print the plan as JSON, in the format of docs/plan-format.md."))
            <*> output logLevelParser
            <*> many (parsecArgument (metavar "TARGET..." <> help "A package to plan for, e.g. aeson, or aeson-2.2.0.0 for that version."))

    packages = parserOptionGroup "Packages:"
    solver = parserOptionGroup "Solver:"
    output = parserOptionGroup "Output:"

    mods =
        mconcat
            [ fullDesc
            , header "cabal-solver-plan - run cabal-install's solver on inputs given on the command line"
            , progDesc description
            , footerDoc . Just $
                vsep
                    [ examples
                    , mempty
                    , pretty "Exit status: 0 a plan was found, 1 no plan was found, 2 usage error, 3 other error."
                    , mempty
                    , pretty "Documentation: https://github.com/andreabedini/cabal-solver-plan"
                    , pretty "The --json format: https://github.com/andreabedini/cabal-solver-plan/blob/main/docs/plan-format.md"
                    , pretty "Report bugs: https://github.com/andreabedini/cabal-solver-plan/issues"
                    ]
            ]

extraPreInstalledParser :: Parser (PackageId, ComponentName)
extraPreInstalledParser =
    parsecOptionWith
        g
        ( long "with-preinstalled"
            <> metavar "PKGID[:LIB]"
            <> help "Treat package PKGID (its main library, or LIB) as installed, though no package db has it, e.g. foo-1.0 or foo-1.0:bar. Not with --json."
            <> hidden
        )
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
            <> help "Download (or update) the secure repository at URI into --cache-dir, and use its packages, e.g. hackage.haskell.org:https://hackage.haskell.org/. Without a NAME, it is called repository1, repository2, ..."
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
    , useGlobalPackageDb :: Bool
    -- ^ The deprecated --use-global-packagedb, which asks for the default.
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
        | noGlobalPackageDb && useGlobalPackageDb ->
            Left "--use-global-packagedb and --no-global-package-db contradict each other: give one of them."
        | otherwise ->
            Right (CompilerFromSystem (fromMaybe "ghc" withCompiler) (not noGlobalPackageDb))
    Just compilerId
        | Just _ <- withCompiler ->
            Left $
                "--with-compiler and --compiler-id cannot be used together: --with-compiler runs"
                    ++ " a GHC to find its id, platform and global package db, while --compiler-id"
                    ++ " describes a compiler without running it."
        | noGlobalPackageDb || useGlobalPackageDb ->
            Left $
                (if noGlobalPackageDb then "--no-global-package-db" else "--use-global-packagedb")
                    ++ " goes with --with-compiler. With --compiler-id there is"
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
                <> hidden
                <> help "Do not load the global package db of --with-compiler's GHC."
            )
        -- The global package db is loaded by default now.
        <*> switch (long "use-global-packagedb" <> internal)
        <*> optional
            ( parsecOption
                ( long "compiler-id"
                    <> hidden
                    <> metavar "COMPILER-ID"
                    <> help "Plan for this compiler, e.g. ghc-9.12.2, without running it. There are no installed packages unless given with --package-db."
                )
            )
        <*> optional
            ( parsecOption
                ( long "abi-tag"
                    <> hidden
                    <> metavar "ABI-TAG"
                    <> help "With --compiler-id: the tag that tells apart incompatible ABIs on the same platform (default: none)."
                )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "compiler-id-compat"
                        <> hidden
                        <> metavar "COMPILER-ID,..."
                        <> help "With --compiler-id: other compilers it is compatible with."
                    )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "supported-language"
                        <> hidden
                        <> metavar "LANGUAGE,..."
                        <> help "With --compiler-id: the language standards it supports (default: all)."
                    )
            )
        <*> optional
            ( unCommaSeparated
                <$> parsecOption
                    ( long "supported-extension"
                        <> hidden
                        <> metavar "EXTENSION,..."
                        <> help "With --compiler-id: the extensions it supports (default: all)."
                    )
            )
        <*> optional
            ( parsecOption
                ( long "platform"
                    <> hidden
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
            , useGlobalPackageDb = useGlobal
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
            <> help "Assume that pkg-config module NAME (at VERSION) is installed."
            <> metavar "NAME[=VERSION]"
            <> hidden
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
                <$> switchPair
                    "reorder-goals"
                    False
                    "Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
            )
        <*> ( CountConflicts
                <$> switchPair
                    "count-conflicts"
                    True
                    "Try to speed up solving by preferring goals that are involved in a lot of conflicts."
            )
        <*> ( FineGrainedConflicts
                <$> switchPair
                    "fine-grained-conflicts"
                    True
                    "Skip a version of a package if it does not resolve the conflicts encountered in the last version, as a solver optimization."
            )
        <*> ( MinimizeConflictSet
                <$> switchPair
                    "minimize-conflict-set"
                    False
                    "When there is no solution, try to improve the error message by finding a minimal conflict set. May increase run time significantly."
            )
        <*> ( IndependentGoals
                <$> switchPair
                    "independent-goals"
                    False
                    "Treat several goals on the command line as independent. If several goals depend on the same package, different versions can be chosen."
            )
        <*> ( AvoidReinstalls
                <$> switchPair
                    "avoid-reinstalls"
                    False
                    "Do not select versions that would destructively overwrite installed packages."
            )
        <*> ( ShadowPkgs
                <$> switchPair
                    "shadow-installed-packages"
                    False
                    "If multiple package instances of the same version are installed, treat all but one as shadowed."
            )
        <*> ( StrongFlags
                <$> switchPair
                    "strong-flags"
                    False
                    "Do not defer flag choices (this used to be the default in cabal-install <= 1.20)."
            )
        <*> flag
            OnlyConstrainedNone
            OnlyConstrainedAll
            ( long "reject-unconstrained-dependencies"
                <> hidden
                <> help "Require all packages to have constraints on them if they are to be selected."
            )
        <*> ( (\case n | n < 0 -> Nothing; n -> Just n)
                <$> option
                    auto
                    ( long "max-backjumps"
                        <> hidden
                        <> metavar "N"
                        <> help "Maximum number of backjumps allowed while solving. Use a negative number to enable unlimited backtracking. Use 0 to disable backtracking completely."
                        <> value defaultMaxBackjumps
                        <> showDefault
                    )
            )
        -- As in cabal-install, backjumping is always on.
        <*> pure (EnableBackjumping True)
        <*> ( SolveExecutables
                <$> switchPair
                    "solve-executables"
                    True
                    "Solve for the executables that packages need to build (build-tool-depends)."
            )
        -- Skip goalOrder. It is a function
        <*> pure Nothing
        -- At verbose, the explanation of a failure would repeat the whole
        -- trace, which --verbose prints already.
        <*> pure (Verbosity.vLevel Verbosity.normal)
        -- cabal-install sets it only to find a minimal conflict set.
        <*> pure (PruneAfterFirstSuccess False)

-- | A setting that --NAME turns on and --no-NAME turns off; the last one
-- given wins. Only --NAME is listed in the help.
switchPair :: String -> Bool -> String -> Parser Bool
switchPair name def doc =
    maybe def NE.last . NE.nonEmpty
        <$> many
            ( flag' True (long name <> help (doc ++ defaultText) <> hidden)
                <|> flag' False (long ("no-" ++ name) <> internal)
            )
  where
    defaultText
        | def = " On by default; turn it off with --no-" ++ name ++ "."
        | otherwise = " Off by default."

-- | The least severe messages to print: -v adds the solver's trace, -q
-- leaves out what is printed while working.
logLevelParser :: Parser Severity
logLevelParser =
    flag' Debug (short 'v' <> long "verbose" <> help "Also print the solver's trace." <> hidden)
        <|> flag' Warning (short 'q' <> long "quiet" <> help "Print only warnings and errors." <> hidden)
        <|> pure Info

flagAssignmentParser :: Parser (Map PackageName FlagAssignment)
flagAssignmentParser =
    Map.fromListWith (<>)
        <$> many
            ( parsecOptionWith
                parser
                ( long "flag"
                    <> metavar "PKG:FLAGS"
                    <> help "Set the flags of a package, e.g. text:-simdutf or 'foo:+a -b'."
                    <> hidden
                )
            )
  where
    parser = (,) <$> Parsec.parsec <* P.char ':' <*> parsecFlagAssignmentNonEmpty

parsecOption :: (Parsec.Parsec a) => Mod OptionFields a -> Parser a
parsecOption = option (parsecReader Parsec.parsec)

parsecOptionWith :: Parsec.ParsecParser a -> Mod OptionFields a -> Parser a
parsecOptionWith parser = option (parsecReader parser)

parsecArgument :: (Parsec.Parsec a) => Mod ArgumentFields a -> Parser a
parsecArgument = argument (parsecReader Parsec.parsec)

-- | Read a value with a Cabal parser. Its errors start with a made-up file
-- name and a line number, which say nothing for a command-line argument.
parsecReader :: Parsec.ParsecParser a -> ReadM a
parsecReader parser = eitherReader $ \s ->
    case Parsec.explicitEitherParsec parser s of
        Right a -> Right a
        Left err -> Left ("cannot parse " ++ show s ++ ": " ++ tidy err)
  where
    tidy err = intercalate "; " (filter (not . null) (lines (afterPosition err)))
    afterPosition err = case dropWhile (/= ')') err of
        ')' : ':' : rest -> rest
        _ -> err

newtype CommaSeparated a = CommaSeparated {unCommaSeparated :: [a]}

instance (Cabal.Parsec a) => Cabal.Parsec (CommaSeparated a) where
    parsec = CommaSeparated <$> (Cabal.parsec `P.sepBy` P.char ',')
