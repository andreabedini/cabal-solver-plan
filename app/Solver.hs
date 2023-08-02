{-# LANGUAGE MultiWayIf #-}

module Solver where

import Control.Monad.Writer.CPS (MonadWriter (..), execWriter)
import Data.Foldable
import Data.List (nub)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Client.Dependency hiding (removeLowerBounds, removeUpperBounds)
import Distribution.Client.Targets (UserConstraint, userToPackageConstraint)
import Distribution.Client.Utils (incVersion)
import Distribution.PackageDescription qualified as PD hiding (setupBuildInfo)
import Distribution.Simple as Cabal hiding (installedUnitId)
import Distribution.Simple.Flag qualified as Flag
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Solver.Modular
import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.IndexConversion
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.RetryLog (RetryLog)
import Distribution.Solver.Modular.Solver (solve)
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.InstalledPreference
import Distribution.Solver.Types.InstalledPreference qualified as Preference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint (ConstraintScope (..), scopeToPackageName)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.SourcePackage (SourcePackage)
import Distribution.System (Arch, OS)
import Distribution.Types.PackageVersionConstraint
import SetupDeps (cabalPkgname)

compute ::
  CompilerInfo ->
  OS ->
  Arch ->
  PkgConfigDb ->
  [(UserConstraint, ConstraintSource)] ->
  [PackageVersionConstraint] ->
  Map PackageName PD.FlagAssignment ->
  SolverConfig ->
  PreferOldest ->
  InstalledPackageIndex ->
  PackageIndex (SourcePackage loc) ->
  Map PackageName VersionRange ->
  Set PackageName ->
  RetryLog Message SolverFailure (Assignment, RevDepMap)
compute
  cinfo
  os
  arch
  pkgConfigDb
  solverSettingConstraints
  solverSettingPreferences
  solverSettingFlagAssignments
  solverConfig
  solverSettingPreferOldest
  installedPkgIndex
  sourcePkgIndex
  sourcePkgPrefs
  targets =
    solve solverConfig cinfo idx pkgConfigDb packagePreferences gcs targets
    where
      constraints :: [LabeledPackageConstraint]
      constraints = mkConstraints (compilerInfoId cinfo) solverSettingConstraints solverSettingFlagAssignments

      packagePreferences :: PackageName -> PackagePreferences
      packagePreferences = interpretPackagesPreference targets preferenceDefault preferences

      preferences :: [PackagePreference]
      preferences = mkPreferences sourcePkgPrefs solverSettingPreferences

      preferenceDefault :: PackagesPreferenceDefault
      preferenceDefault =
        -- TODO: [required eventually] decide if we need to prefer
        -- installed for global packages, or prefer latest even for
        -- global packages. Perhaps should be configurable but with a
        -- different name than "upgrade-dependencies".
        if Flag.asBool solverSettingPreferOldest
          then PreferAllOldest
          else PreferLatestForSelected

      -- Constraints have to be converted into a finite map indexed by PN.
      gcs :: Map.Map PackageName [LabeledPackageConstraint]
      gcs =
        Map.fromListWith
          (<>)
          [ (scopeToPackageName scope, [lpc])
            | lpc@(LabeledPackageConstraint (PackageConstraint scope _property) _source) <- constraints
          ]

      idx :: Index
      idx =
        convPIs
          os
          arch
          cinfo
          gcs
          (ShadowPkgs False)
          (StrongFlags False)
          (SolveExecutables True)
          installedPkgIndex
          sourcePkgIndex

mkConstraints ::
  -- | compiler
  CompilerId ->
  -- | ssConstraints
  [(UserConstraint, ConstraintSource)] ->
  -- | ssFlagAssignments (?)
  Map.Map PackageName PD.FlagAssignment ->
  -- | result
  [LabeledPackageConstraint]
mkConstraints compilerId ssConstraints ssFlagAssignments =
  execWriter $ do
    tell
      [ LabeledPackageConstraint
          (PackageConstraint (ScopeAnySetupQualifier cabalPkgname) (PackagePropertyVersion $ orLaterVersion $ setupMinCabalVersion compilerId))
          ConstraintSetupCabalMinVersion
      ]

    tell
      [ LabeledPackageConstraint
          (PackageConstraint (ScopeAnySetupQualifier cabalPkgname) (PackagePropertyVersion $ earlierVersion setupMaxCabalVersion))
          ConstraintSetupCabalMaxVersion
      ]

    -- version constraints from the config file or command line
    for_ ssConstraints $ \(pc, src) ->
      tell [LabeledPackageConstraint (userToPackageConstraint pc) src]

    -- TODO: [nice to have] should have checked at some point that the package in question actually has these flags.
    for_ (Map.toList ssFlagAssignments) $ \(pkgname, flags) ->
      tell
        [ LabeledPackageConstraint
            (PackageConstraint (scopeToplevel pkgname) (PackagePropertyFlags flags))
            ConstraintSourceConfigFlagOrTarget
        ]

mkPreferences ::
  -- | sourcePkgPrefs
  Map.Map PackageName VersionRange ->
  -- | ssPreferences
  [PackageVersionConstraint] ->
  -- | result
  [PackagePreference]
mkPreferences sourcePkgPrefs ssPreferences =
  -- preferences from the config file or command line
  [PackageVersionPreference name ver | PackageVersionConstraint name ver <- ssPreferences]
    ++ [PackageVersionPreference name ver | (name, ver) <- Map.toList sourcePkgPrefs]

-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
interpretPackagesPreference ::
  Set PackageName ->
  PackagesPreferenceDefault ->
  [PackagePreference] ->
  (PackageName -> PackagePreferences)
interpretPackagesPreference selected defaultPref prefs =
  \pkgname ->
    PackagePreferences
      (versionPref pkgname)
      (installPref pkgname)
      (stanzasPref pkgname)
  where
    versionPref :: PackageName -> [VersionRange]
    versionPref pkgname =
      fromMaybe [anyVersion] (Map.lookup pkgname versionPrefs)

    versionPrefs :: Map.Map PackageName [VersionRange]
    versionPrefs =
      Map.fromListWith
        (++)
        [(pkgname, [pref]) | PackageVersionPreference pkgname pref <- prefs]

    installPref :: PackageName -> InstalledPreference
    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)

    installPrefs :: Map.Map PackageName InstalledPreference
    installPrefs =
      Map.fromList
        [(pkgname, pref) | PackageInstalledPreference pkgname pref <- prefs]

    installPrefDefault :: PackageName -> InstalledPreference
    installPrefDefault = case defaultPref of
      PreferAllLatest -> const Preference.PreferLatest
      PreferAllOldest -> const Preference.PreferOldest
      PreferAllInstalled -> const Preference.PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected
          then Preference.PreferLatest
          else Preference.PreferInstalled

    stanzasPref :: PackageName -> [OptionalStanza]
    stanzasPref pkgname =
      fromMaybe [] (Map.lookup pkgname stanzasPrefs)

    stanzasPrefs :: Map.Map PackageName [OptionalStanza]
    stanzasPrefs =
      Map.fromListWith
        (\a b -> nub (a ++ b))
        [ (pkgname, pref)
          | PackageStanzasPreference pkgname pref <- prefs
        ]

-- | Append the given package databases to an existing PackageDBStack.
-- A @Nothing@ entry will clear everything before it.
applyPackageDbFlags :: PackageDBStack -> [Maybe PackageDB] -> PackageDBStack
applyPackageDbFlags dbs' [] = dbs'
applyPackageDbFlags _ (Nothing : dbs) = applyPackageDbFlags [] dbs
applyPackageDbFlags dbs' (Just db : dbs) = applyPackageDbFlags (dbs' ++ [db]) dbs

-- While we can talk to older Cabal versions (we need to be able to
-- do so for custom Setup scripts that require older Cabal lib
-- versions), we have problems talking to some older versions that
-- don't support certain features.
--
-- For example, Cabal-1.16 and older do not know about build targets.
-- Even worse, 1.18 and older only supported the --constraint flag
-- with source package ids, not --dependency with installed package
-- ids. That is bad because we cannot reliably select the right
-- dependencies in the presence of multiple instances (i.e. the
-- store). See issue #3932. So we require Cabal 1.20 as a minimum.
--
-- Moreover, lib:Cabal generally only supports the interface of
-- current and past compilers; in fact recent lib:Cabal versions
-- will warn when they encounter a too new or unknown GHC compiler
-- version (c.f. #415). To avoid running into unsupported
-- configurations we encode the compatibility matrix as lower
-- bounds on lib:Cabal here (effectively corresponding to the
-- respective major Cabal version bundled with the respective GHC
-- release).
--
-- etc.
-- GHC 9.2   needs  Cabal >= 3.6
-- GHC 9.0   needs  Cabal >= 3.4
-- GHC 8.10  needs  Cabal >= 3.2
-- GHC 8.8   needs  Cabal >= 3.0
-- GHC 8.6   needs  Cabal >= 2.4
-- GHC 8.4   needs  Cabal >= 2.2
-- GHC 8.2   needs  Cabal >= 2.0
-- GHC 8.0   needs  Cabal >= 1.24
-- GHC 7.10  needs  Cabal >= 1.22
--
-- (NB: we don't need to consider older GHCs as Cabal >= 1.20 is
-- the absolute lower bound)
--
-- TODO: long-term, this compatibility matrix should be
--       stored as a field inside 'Distribution.Compiler.Compiler'
setupMinCabalVersion :: CompilerId -> Version
setupMinCabalVersion (CompilerId flavor version) =
  if
      | isGHC, version >= mkVersion [9, 6] -> mkVersion [3, 10]
      | isGHC, version >= mkVersion [9, 4] -> mkVersion [3, 8]
      | isGHC, version >= mkVersion [9, 2] -> mkVersion [3, 6]
      | isGHC, version >= mkVersion [9, 0] -> mkVersion [3, 4]
      | isGHC, version >= mkVersion [8, 10] -> mkVersion [3, 2]
      | isGHC, version >= mkVersion [8, 8] -> mkVersion [3, 0]
      | isGHC, version >= mkVersion [8, 6] -> mkVersion [2, 4]
      | isGHC, version >= mkVersion [8, 4] -> mkVersion [2, 2]
      | isGHC, version >= mkVersion [8, 2] -> mkVersion [2, 0]
      | isGHC, version >= mkVersion [8, 0] -> mkVersion [1, 24]
      | isGHC, version >= mkVersion [7, 10] -> mkVersion [1, 22]
      | otherwise -> mkVersion [1, 20]
  where
    isGHC = flavor `elem` [GHC, GHCJS]

-- As we can't predict the future, we also place a global upper
-- bound on the lib:Cabal version we know how to interact with:
--
-- The upper bound is computed by incrementing the current major
-- version twice in order to allow for the current version, as
-- well as the next adjacent major version (one of which will not
-- be released, as only "even major" versions of Cabal are
-- released to Hackage or bundled with proper GHC releases).
--
-- For instance, if the current version of cabal-install is an odd
-- development version, e.g.  Cabal-2.1.0.0, then we impose an
-- upper bound `setup.Cabal < 2.3`; if `cabal-install` is on a
-- stable/release even version, e.g. Cabal-2.2.1.0, the upper
-- bound is `setup.Cabal < 2.4`. This gives us enough flexibility
-- when dealing with development snapshots of Cabal and cabal-install.
--
setupMaxCabalVersion :: Version
setupMaxCabalVersion =
  alterVersion (take 2) $ incVersion 1 $ incVersion 1 cabalVersion
