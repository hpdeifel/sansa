{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Aria2.Commands
       ( Command
       , addUris
       , pause
       , pauseAll
       , forcePause
       , forcePauseAll
       , unpause
       , unpauseAll
       , remove
       , forceRemove
       ) where

import Aria2.Types
import Data.Aeson
import JsonRPC
import Network.URI
import qualified Data.Text as T
import Data.Text (Text)

aria2Path :: String
aria2Path = "/jsonrpc"

mkUri :: Host -> Port -> URI
mkUri (Host host) (Port port) =
  URI "http:" (Just $ URIAuth "" host' port') aria2Path "" ""

  where host' = T.unpack host
        port' = ":" ++ T.unpack port

resultToEither :: Result a -> Either Text a
resultToEither (Error err) = Left $ T.pack err
resultToEither (Success a) = Right a

command :: FromJSON a =>
           MethodName -> [Value] -> Host -> Port -> IO (Either Text a)
command (MethodName meth) args host port = do
  res <- sendRequest' uri request
  return $ res >>= \res' -> case errorObj res' of
    Null -> resultToEither $ fromJSON (result res')
    err    -> either Left Left $ resultToEither $ fromJSON err

  where uri = mkUri host port
        request = Request {
          method = meth,
          arguments = args,
          requestId = ""
        }


type Command a = Host -> Port -> IO (Either Text a)

addUris :: [URI] -> Command GID
addUris uris = command "aria2.addUri" [toJSON (map showUri uris)]
  where showUri uri = uriToString id uri ""

pause :: GID -> Command GID
pause gid = command "aria2.pause" [toJSON gid]

pauseAll :: Command OK
pauseAll = command "aria2.pauseAll" []

forcePause :: GID -> Command GID
forcePause gid = command "aria2.forcePause" [toJSON gid]

forcePauseAll :: Command GID
forcePauseAll = command "aria2.forcePauseAll" []

unpause :: GID -> Command GID
unpause gid = command "aria2.unpause" [toJSON gid]

unpauseAll :: Command GID
unpauseAll = command "aria2.unpauseAll" []

remove :: GID -> Command GID
remove gid = command "aria2.remove" [toJSON gid]

forceRemove :: GID -> Command GID
forceRemove gid = command "aria2.forceRemove" [toJSON gid]
