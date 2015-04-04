{-# LANGUAGE OverloadedStrings, LambdaCase, TypeSynonymInstances,
             FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
       , tellStatus
       , tellActive
       , tellWaiting
       , tellStopped
       ) where

import Aria2.Types
import Data.Aeson
import JsonRPC
import Network.URI
import Network.URI.Json ()
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

commandImpl :: FromJSON a => MethodName -> [Value] -> Command a
commandImpl (MethodName meth) args = do
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

class CmdType r where
  execCmd :: MethodName -> [Value] -> r

instance FromJSON a => CmdType (Command a) where
  execCmd = commandImpl

instance (CmdType r, ToJSON a) => CmdType (a -> r) where
  execCmd name args arg1 = execCmd name (args ++ [toJSON arg1])

command :: (CmdType r) => MethodName -> r
command name = execCmd name []

addUris :: [URI] -> Command GID
addUris = command "aria2.addUri"

pause :: GID -> Command GID
pause = command "aria2.pause"

pauseAll :: Command OK
pauseAll = command "aria2.pauseAll"

forcePause :: GID -> Command GID
forcePause = command "aria2.forcePause"

forcePauseAll :: Command GID
forcePauseAll = command "aria2.forcePauseAll"

unpause :: GID -> Command GID
unpause = command "aria2.unpause"

unpauseAll :: Command GID
unpauseAll = command "aria2.unpauseAll"

remove :: GID -> Command GID
remove = command "aria2.remove"

forceRemove :: GID -> Command GID
forceRemove = command "aria2.forceRemove"

tellStatus :: GID -> Command DownloadInfo
tellStatus = command "aria2.tellStatus"

tellActive :: Command [DownloadInfo]
tellActive = command "aria2.tellActive"

tellWaiting :: Command [DownloadInfo]
tellWaiting = command "aria2.tellWaiting"

tellStopped :: Command [DownloadInfo]
tellStopped = command "aria2.tellStopped"
