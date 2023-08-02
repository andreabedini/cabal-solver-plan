{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Client.Targets
import Distribution.Client.Types.PackageLocation
import Distribution.Compat.CharParsing qualified as P
import Distribution.Compat.Graph (nodeKey)
import Distribution.InstalledPackageInfo (InstalledPackageInfo, parseInstalledPackageInfo)
import Distribution.Parsec (Parsec, eitherParsec, explicitEitherParsec, parsec)
import Distribution.Simple
import Distribution.Simple.PackageIndex qualified as Cabal
import Distribution.Simple.Utils (ordNubBy)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (PruneAfterFirstSuccess), SolverConfig (..))
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageIndex as Solver
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

data Arguments = Arguments
  { compiler :: CompilerInfo,
    os :: OS,
    arch :: Arch,
    pkgConfigDb :: PkgConfigDb,
    constraints :: [(UserConstraint, ConstraintSource)],
    preferences :: [PackageVersionConstraint],
    flagAssignments :: Map PackageName FlagAssignment,
    solverConfig :: SolverConfig,
    preferOldest :: PreferOldest,
    installedPkgIndex :: Cabal.PackageIndex InstalledPackageInfo,
    sourcePkgIndex :: Solver.PackageIndex UnresolvedSourcePackage,
    sourcePkgPrefs :: Map PackageName VersionRange,
    targets :: Set PackageName
  }

opts :: ParserInfo Arguments
opts = info (s <**> helper) fullDesc
  where
    s :: Parser Arguments
    s =
      Arguments
        <$> ( CompilerInfo
                <$> option (eitherReader eitherParsec) (long "compiler-id")
                <*> option (eitherReader eitherParsec) (long "abi-tag" <> value NoAbiTag)
                <*> pure Nothing
                <*> optional (some (option (eitherReader eitherParsec) (long "language")))
                <*> optional (some (option (eitherReader eitherParsec) (long "extension")))
            )
        <*> option (eitherReader eitherParsec) (long "os" <> metavar "OS")
        <*> option (eitherReader eitherParsec) (long "arch" <> metavar "ARCH")
        <*> pkgConfigDbParser
        <*> many constraintsParser
        <*> pure [] -- TODO: solverSettingPreferences
        <*> flagAssignmentParser
        <*> solverConfigParser
        <*> (PreferOldest <$> switch (long "prefer-oldest"))
        <*> pure mempty -- FIXME: installedPkgIndex
        <*> pure mempty -- FIXME: sourcePkgIndex
        <*> pure mempty -- FIXME: sourcePkgPrefs
        <*> (Set.fromList <$> many (argument (eitherReader eitherParsec) (metavar "TARGET")))

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

pkgConfigDbParser :: Parser PkgConfigDb
pkgConfigDbParser =
  PkgConfigDb . Map.fromList <$> many (option entryParser (long "pkgconf"))
  where
    entryParser = eitherReader $ explicitEitherParsec $ do
      pkgName <- parsec
      pkgVersion <- optional (P.string "=" *> parsec)
      return (pkgName, pkgVersion)

parsecOption :: (Parsec a) => Mod OptionFields a -> Parser a
parsecOption = option (eitherReader eitherParsec)

main :: IO ()
main = do
  Arguments {..} <- execParser opts

  let packagedb = "/home/andrea/.ghcup/ghc/9.4.5/lib64/ghc-9.4.5/lib/package.conf.d"
  listDirectory packagedb
    >>= traverse (fmap parseInstalledPackageInfo . BS.readFile . (packagedb </>)) . filter ((== ".conf") . takeExtension)
    >>= traverse print

  -- let sourcePackages :: Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
  --     sourcePackages =
  --       removeLowerBounds solverSettingAllowOlder $
  --         removeUpperBounds solverSettingAllowNewer $
  --           applyDefaultSetupDeps compiler platform <$> sourcePkgIndex

  let retryLog =
        compute
          compiler
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
    $ toProgress retryLog
