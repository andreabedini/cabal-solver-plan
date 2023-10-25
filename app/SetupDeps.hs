{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module SetupDeps where

import Distribution.CabalSpecVersion
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types.PackageLocation
import Distribution.Compat.Lens
import Distribution.Package
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty qualified as Cabal
import Distribution.Simple.Compiler
import Distribution.System
import Distribution.Types.GenericPackageDescription.Lens
import Distribution.Types.Lens
import Distribution.Version
import SourcePackage.Lens

applyDefaultSetupDeps ::
    Compiler ->
    Platform ->
    UnresolvedSourcePackage ->
    UnresolvedSourcePackage
applyDefaultSetupDeps comp platform srcpkg =
    over
        (srcpkgDescription . packageDescription . setupBuildInfo)
        ( \case
            Just sbi ->
                Just sbi
            Nothing ->
                case defaultSetupDeps comp platform (srcpkg ^. srcpkgDescription . packageDescription) of
                    Nothing ->
                        Nothing
                    Just deps
                        | isCustom -> Just PD.SetupBuildInfo{PD.defaultSetupDepends = True, PD.setupDepends = deps}
                        | otherwise -> Nothing
        )
        srcpkg
  where
    isCustom = PD.buildType pkgdesc == PD.Custom
    gpkgdesc = view srcpkgDescription srcpkg
    pkgdesc = view packageDescription gpkgdesc

-- | Part of our Setup.hs handling policy is implemented by getting the solver
-- to work out setup dependencies for packages. The solver already handles
-- packages that explicitly specify setup dependencies, but we can also tell
-- the solver to treat other packages as if they had setup dependencies.
-- That's what this function does, it gets called by the solver for all
-- packages that don't already have setup dependencies.
--
-- The dependencies we want to add is different for each 'SetupScriptStyle'.
--
-- Note that adding default deps means these deps are actually /added/ to the
-- packages that we get out of the solver in the 'SolverInstallPlan'. Making
-- implicit setup deps explicit is a problem in the post-solver stages because
-- we still need to distinguish the case of explicit and implicit setup deps.
-- See 'rememberImplicitSetupDeps'.
--
-- Note in addition to adding default setup deps, we also use
-- 'addSetupCabalMinVersionConstraint' (in 'planPackages') to require
-- @Cabal >= 1.20@ for Setup scripts.
defaultSetupDeps ::
    Compiler ->
    Platform ->
    PD.PackageDescription ->
    Maybe [Dependency]
defaultSetupDeps compiler platform pkg =
    case packageSetupScriptStyle pkg of
        -- For packages with build type custom that do not specify explicit
        -- setup dependencies, we add a dependency on Cabal and a number
        -- of other packages.
        SetupCustomImplicitDeps ->
            Just $
                [ Dependency depPkgname anyVersion mainLibSet
                | depPkgname <- legacyCustomSetupPkgs compiler platform
                ]
                    ++ [ Dependency cabalPkgname cabalConstraint mainLibSet
                       | packageName pkg /= cabalPkgname
                       ]
          where
            -- The Cabal dep is slightly special:
            -- \* We omit the dep for the Cabal lib itself, since it bootstraps.
            -- \* We constrain it to be < 1.25
            --
            -- Note: we also add a global constraint to require Cabal >= 1.20
            -- for Setup scripts (see use addSetupCabalMinVersionConstraint).
            --
            cabalConstraint =
                orLaterVersion (csvToVersion (PD.specVersion pkg))
                    `intersectVersionRanges` earlierVersion cabalCompatMaxVer
            -- The idea here is that at some point we will make significant
            -- breaking changes to the Cabal API that Setup.hs scripts use.
            -- So for old custom Setup scripts that do not specify explicit
            -- constraints, we constrain them to use a compatible Cabal version.
            cabalCompatMaxVer = mkVersion [1, 25]

        -- For other build types (like Simple) if we still need to compile an
        -- external Setup.hs, it'll be one of the simple ones that only depends
        -- on Cabal and base.
        SetupNonCustomExternalLib ->
            Just
                [ Dependency cabalPkgname cabalConstraint mainLibSet
                , Dependency basePkgname anyVersion mainLibSet
                ]
          where
            cabalConstraint = orLaterVersion (csvToVersion (PD.specVersion pkg))

        -- The internal setup wrapper method has no deps at all.
        SetupNonCustomInternalLib -> Just []
        -- This case gets ruled out by the caller, planPackages, see the note
        -- above in the SetupCustomImplicitDeps case.
        SetupCustomExplicitDeps ->
            error $
                "defaultSetupDeps: called for a package with explicit "
                    ++ "setup deps: "
                    ++ Cabal.prettyShow (packageId pkg)
  where
    -- we require one less
    --
    -- This maps e.g. CabalSpecV3_0 to mkVersion [2,5]
    csvToVersion :: CabalSpecVersion -> Version
    csvToVersion = mkVersion . cabalSpecMinimumLibraryVersion

---------------------------
-- Setup.hs script policy
--

-- Handling for Setup.hs scripts is a bit tricky, part of it lives in the
-- solver phase, and part in the elaboration phase. We keep the helper
-- functions for both phases together here so at least you can see all of it
-- in one place.
--
-- There are four major cases for Setup.hs handling:
--
--  1. @build-type@ Custom with a @custom-setup@ section
--  2. @build-type@ Custom without a @custom-setup@ section
--  3. @build-type@ not Custom with @cabal-version >  $our-cabal-version@
--  4. @build-type@ not Custom with @cabal-version <= $our-cabal-version@
--
-- It's also worth noting that packages specifying @cabal-version: >= 1.23@
-- or later that have @build-type@ Custom will always have a @custom-setup@
-- section. Therefore in case 2, the specified @cabal-version@ will always be
-- less than 1.23.
--
-- In cases 1 and 2 we obviously have to build an external Setup.hs script,
-- while in case 4 we can use the internal library API.
--
-- TODO:In case 3 we should fail. We don't know how to talk to
-- newer ./Setup.hs
--
-- data SetupScriptStyle = ...  -- see ProjectPlanning.Types

-- | Work out the 'SetupScriptStyle' given the package description.
packageSetupScriptStyle :: PD.PackageDescription -> SetupScriptStyle
packageSetupScriptStyle pkg
    | buildType == PD.Custom
    , Just setupbi <- PD.setupBuildInfo pkg -- does have a custom-setup stanza
    , not (PD.defaultSetupDepends setupbi) -- but not one we added internally
        =
        SetupCustomExplicitDeps
    | buildType == PD.Custom
    , Just setupbi <- PD.setupBuildInfo pkg -- we get this case post-solver as
    , PD.defaultSetupDepends setupbi -- the solver fills in the deps
        =
        SetupCustomImplicitDeps
    | buildType == PD.Custom
    , Nothing <- PD.setupBuildInfo pkg -- we get this case pre-solver
        =
        SetupCustomImplicitDeps
    -- here we should fail.
    | PD.specVersion pkg > cabalSpecLatest -- one cabal-install is built against
        =
        SetupNonCustomExternalLib
    | otherwise =
        SetupNonCustomInternalLib
  where
    buildType = PD.buildType pkg

legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
    map mkPackageName $
        [ "array"
        , "base"
        , "binary"
        , "bytestring"
        , "containers"
        , "deepseq"
        , "directory"
        , "filepath"
        , "pretty"
        , "process"
        , "time"
        , "transformers"
        ]
            ++ ["Win32" | os == Windows]
            ++ ["unix" | os /= Windows]
            ++ ["ghc-prim" | isGHC]
            ++ ["template-haskell" | isGHC]
            ++ ["old-time" | notGHC710]
  where
    isGHC = compilerCompatFlavor GHC compiler
    notGHC710 = case compilerCompatVersion GHC compiler of
        Nothing -> False
        Just v -> v <= mkVersion [7, 9]

basePkgname :: PD.PackageName
basePkgname = mkPackageName "base"

cabalPkgname :: PD.PackageName
cabalPkgname = mkPackageName "cabal"
