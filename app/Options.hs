{-# LANGUAGE LambdaCase #-}

module Options (
    parseOptions,
    Options (..),
    CompilerSource (..),
    PkgConfigDbSource (..),
)
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
import Distribution.Simple (AbiTag (..), CompilerInfo (..), PackageDB (..), PackageDBStack, PackageName, PkgconfigName)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Types.Settings (
    AllowBootLibInstalls (..),
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
    { compilerSource :: CompilerSource
    , extraPreInstalled :: [(PackageId, ComponentName)]
    , repositories :: [Either URI RemoteRepo]
    , mTotalIndexState :: Maybe TotalIndexState
    , pkgConfigDbSources :: [PkgConfigDbSource]
    , constraints :: [UserConstraint]
    , preferences :: [PackageVersionConstraint]
    , flagAssignments :: Map PackageName FlagAssignment
    , solverConfig :: SolverConfig
    , preferOldest :: PreferOldest
    , cacheDir :: FilePath
    , offline :: Bool
    , targets :: [PackageVersionConstraint]
    }

parseOptions :: IO Options
parseOptions = execParser opts

opts :: ParserInfo Options
opts = info (s <**> helper) mods
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

    mods =
        mconcat
            [ fullDesc
            , header "header"
            , footer "footer"
            , progDesc "progDesc"
            ]

extraPreInstalledParser :: Parser (PackageId, ComponentName)
extraPreInstalledParser = parsecOptionWith g (long "with-preinstalled")
  where
    g = do
        pkgId <- Parsec.parsec
        libName <- optional (P.string ":" *> Parsec.parsec)
        return (pkgId, fromMaybe (CLibName LMainLibName) libName)

-- NOTE: all this it to allow users to use a shorter form for repositories
-- cabal requires repo-name:repo-uri but we try to make up a repo-name
-- from the uri. Also not the clearest code.
repositoryParser :: Parser (Either URI RemoteRepo)
repositoryParser = option g (long "repository")
  where
    g = Left <$> maybeReader parseAbsoluteURI <|> Right <$> eitherReader Parsec.eitherParsec

data CompilerSource
    = CompilerInline CompilerInfo Platform [FilePath]
    | CompilerFromSystem FilePath PackageDBStack

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
            <*> many
                ( option
                    readPackageDb
                    ( long "package-db"
                        <> help "Package database of pre-installed packages."
                        <> metavar "GLOBAL|USER|PATH"
                    )
                )

    compilerInline =
        CompilerInline
            <$> compilerInfoOption
            <*> platformOption
            <*> many (strOption (long "package-db-dir" <> metavar "PATH"))

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

    platformOption =
        parsecOption
            ( long "platform"
                <> help "The desired platform for the build plan."
                <> metavar "PLATFORM"
                <> value buildPlatform
                <> showDefaultWith Cabal.prettyShow
            )

    readPackageDb :: ReadM PackageDB
    readPackageDb =
        str >>= \case
            "global" -> return GlobalPackageDB
            "user" -> return UserPackageDB
            path -> return $ SpecificPackageDB path

data PkgConfigDbSource
    = PkgConfigDbEntry PkgconfigName (Maybe PkgconfigVersion)
    | PkgConfigDbFromSystem

pkgConfigDbSourceParser :: Parser PkgConfigDbSource
pkgConfigDbSourceParser =
    asum
        [ parsecOptionWith entryParser (long "with-pkgconfig-entries")
        , flag' PkgConfigDbFromSystem (long "use-system-pkgconfig")
        ]
  where
    entryParser = do
        pkgName <- Parsec.parsec
        pkgVersion <- optional (P.string "=" *> Parsec.parsec)
        return $ PkgConfigDbEntry pkgName pkgVersion

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
        <*> ( AllowBootLibInstalls
                <$> option
                    auto
                    ( long "allow-boot-library-installs"
                        <> help "Allow cabal to install base, ghc-prim, integer-simple, integer-gmp, and template-haskell."
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
        <*> parsecOption
            ( long "verbosity"
                <> value Verbosity.normal
            )
        <*> ( PruneAfterFirstSuccess
                <$> switch
                    ( long "prune-after-first-success"
                    )
            )

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
