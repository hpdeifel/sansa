{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Aria2.Commands
       ( Command
       , ConnectionSettings(..)
       , runCommand
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

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

aria2Path :: String
aria2Path = "/jsonrpc"

mkUri :: Host -> Port -> URI
mkUri (Host h) (Port p) =
  URI "http:" (Just $ URIAuth "" host' port') aria2Path "" ""

  where host' = T.unpack h
        port' = ":" ++ T.unpack p

resultToEither :: Result a -> Either Text a
resultToEither (Error err) = Left $ T.pack err
resultToEither (Success a) = Right a

exceptT :: Monad m => Either a b -> ExceptT a m b
exceptT (Left a)  = throwE a
exceptT (Right b) = return b

data ConnectionSettings = ConSettings {
  host :: Host,
  port :: Port
}

type Command = ReaderT ConnectionSettings (ExceptT Text IO)

runCommand :: ConnectionSettings -> Command a -> IO (Either Text a)
runCommand settings action = runExceptT $ runReaderT action settings

command :: FromJSON a => MethodName -> [Value] -> Command a
command (MethodName meth) args = do
  h <- asks host
  p <- asks port
  res <- liftIO $ sendRequest' (mkUri h p) request
  lift (either throwE return res) >>= \res' -> case errorObj res' of
    Null -> lift $ exceptT $ resultToEither $ fromJSON (result res')
    err  -> lift $ either throwE (throwE.errorMessage) $
              resultToEither $ fromJSON err

  where request = Request {
          method = meth,
          arguments = args,
          requestId = ""
        }


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
