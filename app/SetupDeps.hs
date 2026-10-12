{-# LANGUAGE LambdaCase #-}

-- | Default setup dependencies, added to source packages before solving as
-- cabal-install 3.18 does (see 'Distribution.Client.Dependency.addDefaultSetupDependencies'
-- and 'Distribution.Client.ProjectPlanning.SetupPolicy.mkDefaultSetupDeps').
--
-- cabal-install works these out from a configured 'Compiler'. We only
-- need what is in 'CompilerInfo', which we also have with --compiler-id.
module SetupDeps (
    applyDefaultSetupDeps,
    cabalPkgname,
) where

import Distribution.CabalSpecVersion (cabalSpecMinimumLibraryVersion)
import Distribution.Client.Dependency (setImplicitSetupInfo)
import Distribution.Client.ProjectPlanning.SetupPolicy (packageSetupScriptStyle)
import Distribution.Client.ProjectPlanning.Types (SetupScriptStyle (..))
import Distribution.Compat.Lens (over)
import Distribution.Compiler (CompilerFlavor (..), CompilerId (..), CompilerInfo (..))
import Distribution.Package (Dependency (..), PackageName, depPkgName, mainLibSet, mkPackageName, packageName)
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty qualified as Cabal
import Distribution.Solver.Types.SourcePackage (SourcePackage)
import Distribution.System (OS (Windows), Platform (..))
import Distribution.Types.GenericPackageDescription.Lens (packageDescription)
import Distribution.Version (anyVersion, earlierVersion, intersectVersionRanges, mkVersion, orLaterVersion)
import SourcePackage.Lens (srcpkgDescription)

-- | Give a package without explicit setup dependencies the ones cabal-install
-- would assume for it. Only Custom and Hooks packages are affected.
applyDefaultSetupDeps :: CompilerInfo -> Platform -> SourcePackage loc -> SourcePackage loc
applyDefaultSetupDeps cinfo platform =
    over (srcpkgDescription . packageDescription) $ \pd ->
        pd
            { PD.setupBuildInfo =
                addCabalDepForHooks (PD.buildType pd) $
                    setImplicitSetupInfo (defaultSetupDeps cinfo platform pd) (PD.buildType pd) (PD.setupBuildInfo pd)
            }

-- | A copy of cabal-install's 'mkDefaultSetupDeps'.
defaultSetupDeps :: CompilerInfo -> Platform -> PD.PackageDescription -> Maybe [Dependency]
defaultSetupDeps cinfo platform pkg =
    case packageSetupScriptStyle pkg of
        -- For packages with build type custom that do not specify explicit
        -- setup dependencies, we add a dependency on Cabal and a number
        -- of other packages.
        SetupCustomImplicitDeps ->
            Just $
                [ Dependency depPkgname anyVersion mainLibSet
                | depPkgname <- legacyCustomSetupPkgs cinfo platform
                ]
                    ++ [ Dependency cabalPkgname cabalConstraint mainLibSet
                       | packageName pkg /= cabalPkgname
                       ]
          where
            -- The Cabal dep is slightly special:
            -- \* We omit the dep for the Cabal lib itself, since it bootstraps.
            -- \* We constrain it to be < 1.25
            cabalConstraint =
                orLaterVersion (csvToVersion (PD.specVersion pkg))
                    `intersectVersionRanges` earlierVersion (mkVersion [1, 25])
        -- For other build types (like Simple) if we still need to compile an
        -- external Setup.hs, it'll be one of the simple ones that only depends
        -- on Cabal.
        SetupNonCustomExternalLib ->
            Just [Dependency cabalPkgname (orLaterVersion (csvToVersion (PD.specVersion pkg))) mainLibSet]
        -- The internal setup wrapper method has no deps at all.
        SetupNonCustomInternalLib -> Just []
        -- Never forced: setImplicitSetupInfo only asks for default
        -- dependencies when there are no explicit ones.
        SetupCustomExplicitDeps ->
            error $ "defaultSetupDeps: called for a package with explicit setup deps: " ++ Cabal.prettyShow (PD.package pkg)
  where
    -- This maps e.g. CabalSpecV3_0 to mkVersion [2,5]
    csvToVersion = mkVersion . cabalSpecMinimumLibraryVersion

-- | A copy of cabal-install's 'addCabalDepForHooks': a Hooks package needs
-- Cabal to compile its Setup.hs.
addCabalDepForHooks :: PD.BuildType -> Maybe PD.SetupBuildInfo -> Maybe PD.SetupBuildInfo
addCabalDepForHooks PD.Hooks = fmap addDep
  where
    addDep sbi
        | any ((== cabalPkgname) . depPkgName) (PD.setupDepends sbi) = sbi
        | otherwise = sbi{PD.setupDepends = Dependency cabalPkgname anyVersion mainLibSet : PD.setupDepends sbi}
addCabalDepForHooks _ = id

legacyCustomSetupPkgs :: CompilerInfo -> Platform -> [PackageName]
legacyCustomSetupPkgs cinfo (Platform _ os) =
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
  where
    -- As 'compilerCompatFlavor GHC'.
    isGHC = any (\(CompilerId flavor _) -> flavor == GHC) (compilerInfoId cinfo : concat (compilerInfoCompat cinfo))

cabalPkgname :: PackageName
cabalPkgname = mkPackageName "Cabal"
