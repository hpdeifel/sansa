{-# LANGUAGE LambdaCase #-}
module Sansa.Commands.Add
       ( addCmd
       ) where

import qualified Data.Text.IO as T
import System.IO

import Sansa.CommandsCommon
import Aria2.Commands (addUris)
import Aria2.Types

import Network.URI

addCmd :: Command
addCmd = info (helper <*> addOpts)
           (  fullDesc
           <> header "add: Add URLs pointing to a single file for download"
           )

addOpts :: Parser CmdAction
addOpts = addAction <$> some (argument (str >>= readUri) (metavar "URL..."))

addAction :: [URI] -> Options -> IO ()
addAction uris opts = addUris uris (host opts) (port opts) >>= \case
  Left err -> T.hPutStrLn stderr err
  Right (GID gid) -> T.putStrLn gid

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'
