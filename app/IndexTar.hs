{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read a Hackage-style package index (@01-index.tar@) directly.
--
-- cabal-install reads an index through its own cache (@01-index.cache@),
-- whose format depends on the cabal-install version. Here we read the tar
-- file itself: we walk the entry headers (seeking over the contents), keep
-- every @.cabal@ revision, @package.json@ and @preferred-versions@ entry up
-- to the index-state, and only read the contents we need, when we need them.
module IndexTar (
    Repository (..),
    Index (..),
    PackageEntry (..),
    CabalFileInfo (..),
    SdistInfo (..),
    readIndex,
    indexSourcePackages,
    cabalFileInfo,
    sdistInfo,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Tar.Index (TarEntryOffset, hReadEntryHeaderOrEof, hSeekEntryContentOffset)
import Control.Exception (throwIO)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (rights)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Distribution.Client.HashValue (hashValue, showHashValue)
import Distribution.Client.IndexUtils (parsePreferredVersionsWarnings)
import Distribution.Client.IndexUtils.Timestamp (Timestamp (NoTimestamp), epochTimeToTimestamp)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Solver.Types.PackageIndex qualified as PackageIndex
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.PackageId (PackageId, PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.VersionRange (VersionRange, intersectVersionRanges)
import Network.URI (URI)
import System.FilePath.Posix (splitDirectories)
import System.IO (Handle, IOMode (ReadMode), withBinaryFile)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | A package repository, as far as the plan is concerned: where its
-- packages are downloaded from and where its index is.
data Repository = Repository
    { repositoryName :: String
    , repositoryUri :: URI
    , repositoryIndexTar :: FilePath
    }

instance Eq Repository where
    a == b = repositoryName a == repositoryName b

instance Show Repository where
    show = repositoryName

-- | Where an index entry is in the tar file.
data EntryRef = EntryRef
    { entryOffset :: !TarEntryOffset
    , entrySize :: !Int64
    }

data PackageEntry = PackageEntry
    { packageCabalFiles :: ![EntryRef]
    -- ^ All revisions of the .cabal file up to the index-state, latest first.
    , packageJson :: !(Maybe EntryRef)
    -- ^ The latest package.json (hackage-security target metadata).
    }

data Index = Index
    { indexRepository :: Repository
    , indexTimestamp :: Timestamp
    -- ^ The timestamp of the latest entry used, i.e. the effective index-state.
    , indexPackages :: Map PackageId PackageEntry
    , indexPreferences :: Map PackageName VersionRange
    }

-- | Read the entries of an index, up to (and including) the given timestamp.
readIndex :: Repository -> Maybe Timestamp -> IO Index
readIndex repo mCutoff =
    withBinaryFile (repositoryIndexTar repo) ReadMode $ \h -> do
        let included t = maybe True (epochTimeToTimestamp t <=) mCutoff
            go !ts !pkgs !prefs off =
                hReadEntryHeaderOrEof h off >>= \case
                    Nothing -> return (ts, pkgs, prefs)
                    Just (entry, off')
                        | Tar.NormalFile _ size <- Tar.entryContent entry
                        , included (Tar.entryTime entry) -> do
                            let ref = EntryRef off size
                                ts' = max ts (epochTimeToTimestamp (Tar.entryTime entry))
                            case splitDirectories (Tar.fromTarPathToPosixPath (Tar.entryTarPath entry)) of
                                [name, ver, file]
                                    | Just pkgid <- packageId name ver
                                    , file == name ++ ".cabal" ->
                                        go ts' (Map.alter (Just . addCabalFile ref) pkgid pkgs) prefs off'
                                    | Just pkgid <- packageId name ver
                                    , file == "package.json" ->
                                        go ts' (Map.alter (Just . setPackageJson ref) pkgid pkgs) prefs off'
                                [name, "preferred-versions"] -> do
                                    content <- readEntry h ref
                                    let ranges = [vr | Dependency _ vr _ <- rights (parsePreferredVersionsWarnings (LBS.fromStrict content))]
                                        prefs' = case ranges of
                                            [] -> Map.delete (mkPackageName name) prefs
                                            _ -> Map.insert (mkPackageName name) (foldl1 intersectVersionRanges ranges) prefs
                                    go ts' pkgs prefs' off'
                                _ -> go ts' pkgs prefs off'
                        | otherwise -> go ts pkgs prefs off'
        (ts, pkgs, prefs) <- go NoTimestamp Map.empty Map.empty 0
        return
            Index
                { indexRepository = repo
                , indexTimestamp = ts
                , -- A package.json without a .cabal file is not a package.
                  indexPackages = Map.filter (not . null . packageCabalFiles) pkgs
                , indexPreferences = prefs
                }
  where
    packageId name ver = PackageIdentifier (mkPackageName name) <$> simpleParsec ver

    addCabalFile ref = \case
        Nothing -> PackageEntry [ref] Nothing
        Just pe -> pe{packageCabalFiles = ref : packageCabalFiles pe}

    setPackageJson ref = \case
        Nothing -> PackageEntry [] (Just ref)
        Just pe -> pe{packageJson = Just ref}

-- | The source packages of an index, with each package description read
-- (and parsed) only when the solver looks at it.
indexSourcePackages :: Index -> IO (PackageIndex (SourcePackage Repository))
indexSourcePackages index =
    PackageIndex.fromList
        <$> sequence
            [ do
                gpd <- unsafeInterleaveIO (readDescription pkgid latest)
                return
                    SourcePackage
                        { srcpkgPackageId = pkgid
                        , srcpkgDescription = gpd
                        , srcpkgSource = indexRepository index
                        , srcpkgDescrOverride = Nothing
                        }
            | (pkgid, PackageEntry{packageCabalFiles = latest : _}) <- Map.toList (indexPackages index)
            ]
  where
    readDescription :: PackageId -> EntryRef -> IO GenericPackageDescription
    readDescription pkgid ref = do
        content <- readIndexEntry index ref
        case parseGenericPackageDescriptionMaybe content of
            Just gpd -> return gpd
            Nothing -> fail ("cannot parse the .cabal file of " ++ prettyShow pkgid ++ " in " ++ repositoryIndexTar (indexRepository index))

data CabalFileInfo = CabalFileInfo
    { cabalFileRevision :: Int
    , cabalFileSha256 :: String
    , cabalFileSize :: Int64
    }

-- | The .cabal file of a package as of the index-state. Revision 0 is the
-- file uploaded with the package; each later entry is a new revision.
cabalFileInfo :: Index -> PackageId -> IO (Maybe CabalFileInfo)
cabalFileInfo index pkgid =
    case Map.lookup pkgid (indexPackages index) of
        Just PackageEntry{packageCabalFiles = revisions@(latest : _)} -> do
            content <- readIndexEntry index latest
            return $
                Just
                    CabalFileInfo
                        { cabalFileRevision = length revisions - 1
                        , cabalFileSha256 = showHashValue (hashValue (LBS.fromStrict content))
                        , cabalFileSize = entrySize latest
                        }
        _ -> return Nothing

data SdistInfo = SdistInfo
    { sdistSha256 :: String
    , sdistSize :: Int64
    }

-- | The checksum and size of a package's source tarball, from the
-- hackage-security target metadata (package.json) in the index.
sdistInfo :: Index -> PackageId -> IO (Maybe SdistInfo)
sdistInfo index pkgid =
    case packageJson =<< Map.lookup pkgid (indexPackages index) of
        Nothing -> return Nothing
        Just ref -> do
            content <- readIndexEntry index ref
            case Aeson.eitherDecodeStrict content >>= Aeson.parseEither parseTargets of
                Left err -> fail ("cannot read package.json of " ++ prettyShow pkgid ++ ": " ++ err)
                Right info -> return (Just info)
  where
    tarball = Aeson.Key.fromString ("<repo>/package/" ++ prettyShow pkgid ++ ".tar.gz")
    parseTargets = Aeson.withObject "package.json" $ \o -> do
        signed <- o .: "signed"
        targets <- signed .: "targets"
        target <- targets .: tarball
        hashes <- target .: "hashes"
        SdistInfo <$> hashes .: "sha256" <*> target .: "length"

readIndexEntry :: Index -> EntryRef -> IO BS.ByteString
readIndexEntry index ref =
    withBinaryFile (repositoryIndexTar (indexRepository index)) ReadMode $ \h ->
        readEntry h ref

readEntry :: Handle -> EntryRef -> IO BS.ByteString
readEntry h ref = do
    hSeekEntryContentOffset h (entryOffset ref)
    content <- BS.hGet h (fromIntegral (entrySize ref))
    if BS.length content == fromIntegral (entrySize ref)
        then return content
        else throwIO (userError "truncated index entry")
