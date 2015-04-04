{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Aria2.Types
       ( GID(..)
       , OK(..)
       , Error(..)
       , MethodName(..)
       , Host(..)
       , Port(..)
       , UriInfo(..)
       , FileInfo(..)
       , DownloadInfo(..)
       ) where

import Data.Text (Text)
import Data.String
import Data.Aeson
import Control.Applicative
import Control.Monad
import Network.URI
import Network.URI.Json ()

newtype GID = GID Text

instance ToJSON GID where
  toJSON (GID gid) = toJSON gid

instance FromJSON GID where
  parseJSON (String gid) = pure $ GID gid
  parseJSON _ = mzero

data OK = OK

instance ToJSON OK where
  toJSON OK = toJSON ("OK" :: String)

instance FromJSON OK where
  parseJSON (String "OK") = pure OK
  parseJSON _ = mzero

data Error = Err {
  errorCode :: Int,
  errorMessage :: Text
}

instance FromJSON Error where
  parseJSON (Object v) = Err <$> v .: "code" <*> v .: "message"
  parseJSON _ = mzero

newtype MethodName = MethodName Text
          deriving (IsString)

newtype Host = Host Text
          deriving (IsString)

newtype Port = Port Text
          deriving (IsString)

data Status = StActive | StWaiting | StPaused | StError | StComplete | StRemoved

instance FromJSON Status where
  parseJSON (String str) = case str of
    "active"   -> pure StActive
    "waiting"  -> pure StWaiting
    "paused"   -> pure StPaused
    "error"    -> pure StError
    "complete" -> pure StComplete
    "removed"  -> pure StRemoved
    _          -> mzero
  parseJSON _ = mzero

data URIStatus = USUsed | USWaiting

instance FromJSON URIStatus where
  parseJSON (String "used") = pure USUsed
  parseJSON (String "waiting") = pure USWaiting
  parseJSON _ = mzero

data UriInfo = UriInfo {
  uiURI :: URI,
  uiStatus :: URIStatus
}

instance FromJSON UriInfo where
  parseJSON (Object v) = UriInfo
                     <$> v .: "uri"
                     <*> v .: "status"
  parseJSON _ = mzero

data FileInfo = FileInfo {
  fiIndex :: Int,
  fiPath :: FilePath,
  fiLength :: Int,
  fiCompletedLength :: Int,
  fiSelected :: Bool,
  fiUris :: [UriInfo]
}

instance FromJSON FileInfo where
  parseJSON (Object v) = FileInfo
                     <$> (read <$> v .: "index")
                     <*> v .: "path"
                     <*> (read <$> v .: "length")
                     <*> (read <$> v .: "completedLength")
                     <*> v .: "selected"
                     <*> v .: "uris"

data DownloadInfo = Info {
  diGID :: GID,
  diStatus :: Status,
  diTotalLength :: Int,
  diCompletedLength :: Int,
  diUploadLength :: Int,
  -- TODO bitfield
  diDownloadSpeed :: Int,
  diUploadSpeed :: Int,
  -- TODO infoHash
  -- TODO numSeeders
  -- TODO pieceLength
  -- TODO numPieces
  -- TODO connections
  -- TODO errorCode
  -- TODO followedBy
  -- TODO belongsTo
  diDir :: Maybe FilePath,
  files :: [FileInfo]
  -- TODO bittorrent
}

-- TODO Gracefully fail if integers can't be parsed

instance FromJSON DownloadInfo where
  parseJSON (Object v) = Info
                     <$> v .: "gid"
                     <*> v .: "status"
                     <*> (read <$> v .: "totalLength")
                     <*> (read <$> v .: "completedLength")
                     <*> (read <$> v .: "uploadLength")
                     <*> (read <$> v .: "downloadSpeed")
                     <*> (read <$> v .: "uploadSpeed")
                     <*> (v .:? "dir")
                     <*> v .: "files"
  parseJSON _ = mzero
