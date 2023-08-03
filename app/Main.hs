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
import Distribution.Client.Targets
import Distribution.Client.Types.PackageLocation
import Distribution.Compat.CharParsing qualified as P
import Distribution.Compat.Graph (nodeKey)
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.PackageDescription (PkgconfigVersion)
import Distribution.Parsec (Parsec, eitherParsec, explicitEitherParsec, parsec)
import Distribution.Simple
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.PackageIndex qualified as Cabal.Index
import Distribution.Simple.Program.Db
import Distribution.Simple.Utils (ordNubBy)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (PruneAfterFirstSuccess), SolverConfig (..))
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageIndex as Solver.Index
import Distribution.Solver.Types.PkgConfigDb
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.Solver.Types.Settings
import Distribution.System
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint
import Distribution.Verbosity qualified as Verbosity
import Options.Applicative
import Solver (compute)
import System.Directory
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

data Options = Options
  { compilerSource :: CompilerSource,
    pkgConfigDbSource :: [PkgConfigDbSource],
    constraints :: [(UserConstraint, ConstraintSource)],
    preferences :: [PackageVersionConstraint],
    flagAssignments :: Map PackageName FlagAssignment,
    solverConfig :: SolverConfig,
    preferOldest :: PreferOldest,
    sourcePkgIndex :: Solver.Index.PackageIndex UnresolvedSourcePackage,
    sourcePkgPrefs :: Map PackageName VersionRange,
    targets :: Set PackageName
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
        <*> pure mempty -- FIXME: sourcePkgIndex
        <*> pure mempty -- FIXME: sourcePkgPrefs
        <*> (Set.fromList <$> many (parsecArgument (metavar "TARGET")))

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

-- data CompilerSource
--   = CompilerInline CompilerInfo OS Arch
--   | CompilerFromCompiler (Maybe FilePath)
--
-- compilerSourceParser :: Parser CompilerSource
-- compilerSourceParser =
--   asum
--     [ flag' (CompilerFromCompiler Nothing) (long "with-system-ghc"),
--       CompilerFromCompiler . Just <$> strOption (long "with-ghc"),
--       CompilerInline <$> compilerInfoParser <*> osParser <*> archParser
--     ]
--   where
--     compilerInfoParser =
--       CompilerInfo
--         <$> parsecOption (long "compiler-id")
--         <*> parsecOption (long "abi-tag" <> value NoAbiTag)
--         <*> optional (some (parsecOption (long "compiler-id-compat")))
--         <*> optional (some (parsecOption (long "language")))
--         <*> optional (some (parsecOption (long "extension")))
--     osParser = parsecOption (long "os" <> metavar "OS")
--     archParser = parsecOption (long "arch" <> metavar "ARCH")

data PkgConfigDbSource
  = PkgConfigDbEntry PkgconfigName (Maybe PkgconfigVersion)
  | PkgConfigDbSystem

pkgConfigDbSourceParser :: Parser PkgConfigDbSource
pkgConfigDbSourceParser =
  asum
    [ option entryParser (long "pkgconfig-entry"),
      flag' PkgConfigDbSystem (long "use-system-pkgconfig")
    ]
  where
    entryParser = eitherReader $ explicitEitherParsec $ do
      pkgName <- parsec
      pkgVersion <- optional (P.string "=" *> parsec)
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
    <*> option
      (str >>= \case "none" -> pure OnlyConstrainedNone; "all" -> pure OnlyConstrainedAll; _ -> fail "bad only-constrainted")
      (long "only-constrainted" <> value OnlyConstrainedNone)
    <*> optional (option (maybeReader readMaybe) (long "max-backjumps"))
    <*> (EnableBackjumping <$> switch (long "enable-backjumping"))
    <*> (SolveExecutables <$> switch (long "solve-executables"))
    <*> pure Nothing -- goalOrder
    <*> parsecOption (long "verbosity" <> value Verbosity.normal)
    <*> (PruneAfterFirstSuccess <$> switch (long "prune-after-first-success"))

constraintsParser :: Parser (UserConstraint, ConstraintSource)
constraintsParser =
  (,ConstraintSourceCommandlineFlag) <$> parsecOption (long "constraint")

flagAssignmentParser :: Parser (Map PackageName FlagAssignment)
flagAssignmentParser =
  Map.fromListWith (<>) <$> many (option (eitherReader $ explicitEitherParsec $ (,) <$> parsec <*> parsec) (long "flag"))

parsecOption :: (Parsec a) => Mod OptionFields a -> Parser a
parsecOption = option (eitherReader eitherParsec)

parsecArgument :: (Parsec a) => Mod ArgumentFields a -> Parser a
parsecArgument = argument (eitherReader eitherParsec)

readPackageDb :: FilePath -> IO InstalledPackageIndex
readPackageDb path = do
  filenames <- listDirectory path
  ipis <- for (filter ((== ".conf") . takeExtension) filenames) $
    \filename -> do
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

  -- let sourcePackages :: Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
  --     sourcePackages =
  --       removeLowerBounds solverSettingAllowOlder $
  --         removeUpperBounds solverSettingAllowNewer $
  --           applyDefaultSetupDeps compiler platform <$> sourcePkgIndex

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
    case NE.nonEmpty pkgConfigDbSource of
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
