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
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Compat.Graph (nodeKey)
import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Simple (PackageName, VersionRange, compilerInfo)
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
import Distribution.Solver.Types.PackageIndex qualified as Solver.Index
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..), readPkgConfigDb)
import Distribution.Solver.Types.Progress (foldProgress)
import Distribution.Solver.Types.Settings (PreferOldest (..))
import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint)
import Distribution.Verbosity qualified as Verbosity
import Options
import Options.Applicative
import Solver (compute)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

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
