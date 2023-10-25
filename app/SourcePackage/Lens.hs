module SourcePackage.Lens where

import Distribution.Compat.Lens
import Distribution.Solver.Types.SourcePackage (PackageDescriptionOverride, SourcePackage)
import Distribution.Solver.Types.SourcePackage qualified as T
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageId (PackageId)

srcpkgPackageId :: Lens' (SourcePackage loc) PackageId
srcpkgPackageId f s = fmap (\x -> s{T.srcpkgPackageId = x}) (f (T.srcpkgPackageId s))
{-# INLINE srcpkgPackageId #-}

srcpkgDescription :: Lens' (SourcePackage loc) GenericPackageDescription
srcpkgDescription f s = fmap (\x -> s{T.srcpkgDescription = x}) (f (T.srcpkgDescription s))
{-# INLINE srcpkgDescription #-}

srcpkgSource :: Lens' (SourcePackage loc) loc
srcpkgSource f s = fmap (\x -> s{T.srcpkgSource = x}) (f (T.srcpkgSource s))
{-# INLINE srcpkgSource #-}

srcpkgDescrOverride :: Lens' (SourcePackage loc) PackageDescriptionOverride
srcpkgDescrOverride f s = fmap (\x -> s{T.srcpkgDescrOverride = x}) (f (T.srcpkgDescrOverride s))
{-# INLINE srcpkgDescrOverride #-}
