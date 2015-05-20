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

-- TODO The following options are available in theory

-- all-proxy
-- all-proxy-passwd
-- all-proxy-user
-- allow-overwrite
-- allow-piece-length-change
-- always-resume
-- async-dns
-- auto-file-renaming
-- bt-enable-lpd
-- bt-exclude-tracker
-- bt-external-ip
-- bt-hash-check-seed
-- bt-max-open-files
-- bt-max-peers
-- bt-metadata-only
-- bt-min-crypto-level
-- bt-prioritize-piece
-- bt-remove-unselected-file
-- bt-request-peer-speed-limit
-- bt-require-crypto
-- bt-save-metadata
-- bt-seed-unverified
-- bt-stop-timeout
-- bt-tracker
-- bt-tracker-connect-timeout
-- bt-tracker-interval
-- bt-tracker-timeout
-- check-integrity
-- checksum
-- conditional-get
-- connect-timeout
-- continue
-- dir
-- dry-run
-- enable-async-dns6
-- enable-http-keep-alive
-- enable-http-pipelining
-- enable-mmap
-- enable-peer-exchange
-- file-allocation
-- follow-metalink
-- follow-torrent
-- ftp-passwd
-- ftp-pasv
-- ftp-proxy
-- ftp-proxy-passwd
-- ftp-proxy-user
-- ftp-reuse-connection
-- ftp-type
-- ftp-user
-- hash-check-only
-- header
-- http-accept-gzip
-- http-auth-challenge
-- http-no-cache
-- http-passwd
-- http-proxy
-- http-proxy-passwd
-- http-proxy-user
-- http-user
-- https-proxy
-- https-proxy-passwd
-- https-proxy-user
-- index-out
-- lowest-speed-limit
-- max-connection-per-server
-- max-download-limit
-- max-file-not-found
-- max-resume-failure-tries
-- max-tries
-- max-upload-limit
-- metalink-base-uri
-- metalink-enable-unique-protocol
-- metalink-language
-- metalink-location
-- metalink-os
-- metalink-preferred-protocol
-- metalink-version
-- min-split-size
-- no-file-allocation-limit
-- no-netrc
-- no-proxy
-- out
-- parameterized-uri
-- pause
-- piece-length
-- proxy-method
-- realtime-chunk-checksum
-- referer
-- remote-time
-- remove-control-file
-- retry-wait
-- reuse-uri
-- seed-ratio
-- seed-time
-- select-file
-- split
-- stream-piece-selector
-- timeout
-- uri-selector
-- use-head
-- user-agent

data DlOptions = DlOptions {
  optDir :: Maybe FilePath,
  optOut :: Maybe FilePath
}

instance FromJSON DlOptions where
  parseJSON (Object v) = DlOptions
                     <$> v .:? "dir"
                     <*> v .:? "out"

instance ToJSON DlOptions where
  toJSON opts = object $ catMaybes
                [ ("dir" .=) <$> optDir opts
                , ("out" .=) <$> optOut opts
                ]
