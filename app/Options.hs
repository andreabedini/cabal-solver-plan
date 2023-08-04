{-# LANGUAGE LambdaCase #-}

module Options where

import Data.Map (Map)
import Data.Map qualified as Map
import Distribution.Compat.CharParsing qualified as P
import Distribution.PackageDescription (PkgconfigVersion, parsecFlagAssignmentNonEmpty)
import Distribution.Parsec qualified as Parsec
import Distribution.Simple (AbiTag (..), CompilerInfo (..), PackageDB (..), PackageDBStack, PackageName, PkgconfigName)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.Settings
  ( AllowBootLibInstalls (..),
    AvoidReinstalls (..),
    CountConflicts (..),
    EnableBackjumping (..),
    FineGrainedConflicts (..),
    IndependentGoals (..),
    MinimizeConflictSet (..),
    OnlyConstrained (..),
    ReorderGoals (..),
    ShadowPkgs (..),
    SolveExecutables (..),
    StrongFlags (..),
  )
import Distribution.System (Arch, OS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Verbosity qualified as Verbosity
import Options.Applicative
import Text.Read (readMaybe)

data CompilerSource
  = CompilerInline CompilerInfo OS Arch [FilePath]
  | CompilerFromCompiler (Maybe FilePath) PackageDBStack

compilerSourceParser :: Parser CompilerSource
compilerSourceParser =
  asum
    [ CompilerFromCompiler Nothing
        <$ flag' () (long "with-system-ghc")
        <*> many (option (parsePackageDb <$> str) (long "package-db" <> metavar "GLOBAL|USER|PATH")),
      CompilerFromCompiler . Just
        <$> strOption (long "with-ghc")
        <*> many (option (parsePackageDb <$> str) (long "package-db" <> metavar "GLOBAL|USER|PATH")),
      CompilerInline
        <$> compilerInfoParser
        <*> osParser
        <*> archParser
        <*> many (strOption (long "package-db" <> metavar "PATH"))
    ]
  where
    compilerInfoParser =
      CompilerInfo
        <$> parsecOption (long "compiler-id")
        <*> parsecOption (long "abi-tag" <> value NoAbiTag)
        <*> optional (some (parsecOption (long "compiler-id-compat")))
        <*> optional (some (parsecOption (long "language")))
        <*> optional (some (parsecOption (long "extension")))
    osParser = parsecOption (long "os" <> metavar "OS")
    archParser = parsecOption (long "arch" <> metavar "ARCH")

parsePackageDb :: String -> PackageDB
parsePackageDb "global" = GlobalPackageDB
parsePackageDb "user" = UserPackageDB
parsePackageDb other = SpecificPackageDB other

data PkgConfigDbSource
  = PkgConfigDbEntry PkgconfigName (Maybe PkgconfigVersion)
  | PkgConfigDbSystem

pkgConfigDbSourceParser :: Parser PkgConfigDbSource
pkgConfigDbSourceParser =
  asum
    [ parsecOptionWith entryParser (long "pkgconfig-entry"),
      flag' PkgConfigDbSystem (long "use-system-pkgconfig")
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
