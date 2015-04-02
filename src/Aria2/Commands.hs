{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Aria2.Commands
       ( Command
       , addUris
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
