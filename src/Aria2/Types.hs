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
       , DlOptions(..)
       ) where

import Data.Text (Text)
import Data.String
import Data.Aeson
import Control.Applicative
import Control.Monad
import Network.URI
import Network.URI.Json ()
import Data.Aeson.Types
import Data.Units
import Data.Maybe

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

instance Show Status where
  show StActive = "active"
  show StWaiting = "waiting"
  show StPaused = "paused"
  show StError = "error"
  show StComplete = "complete"
  show StRemoved = "removed"

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

readBool :: Text -> Parser Bool
readBool "true"  = return True
readBool "false" = return False
readBool _       = mzero

instance FromJSON FileInfo where
  parseJSON (Object v) = FileInfo
                     <$> (read <$> v .: "index")
                     <*> v .: "path"
                     <*> (read <$> v .: "length")
                     <*> (read <$> v .: "completedLength")
                     <*> (v .: "selected" >>= readBool)
                     <*> v .: "uris"
  parseJSON _ = mzero

data DownloadInfo = Info {
  diGID :: GID,
  diStatus :: Status,
  diTotalLength :: DataSize,
  diCompletedLength :: DataSize,
  diUploadLength :: DataSize,
  -- TODO bitfield
  diDownloadSpeed :: DataSpeed,
  diUploadSpeed :: DataSpeed,
  -- TODO infoHash
  -- TODO numSeeders
  -- TODO pieceLength
  -- TODO numPieces
  -- TODO connections
  -- TODO errorCode
  -- TODO followedBy
  -- TODO belongsTo
  diDir :: Maybe FilePath,
  diFiles :: [FileInfo]
  -- TODO bittorrent
}

-- TODO Gracefully fail if integers can't be parsed

instance FromJSON DownloadInfo where
  parseJSON (Object v) = Info
                     <$> v .: "gid"
                     <*> v .: "status"
                     <*> (((% Byte) . read) <$> v .: "totalLength")
                     <*> (((% Byte) . read) <$> v .: "completedLength")
                     <*> (((% Byte) . read) <$> v .: "uploadLength")
                     <*> (readBytesPerSecond <$> v .: "downloadSpeed")
                     <*> (readBytesPerSecond <$> v .: "uploadSpeed")
                     <*> (v .:? "dir")
                     <*> v .: "files"
  parseJSON _ = mzero

readBytesPerSecond :: String -> DataSpeed
readBytesPerSecond = (% (Byte :/ Second)) . read


-- TODO A lot more options are available
data DlOptions = DlOptions {
  optDir :: Maybe FilePath
}

instance FromJSON DlOptions where
  parseJSON (Object v) = DlOptions <$> v .:? "dir"

instance ToJSON DlOptions where
  toJSON opts = object $ catMaybes
                [ ("dir" .=) <$> optDir opts ]
