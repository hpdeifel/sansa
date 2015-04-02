{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Aria2.Commands
import Aria2.Types

import System.Environment
import System.IO
import System.Exit
import Network.URI
import Control.Monad
import qualified Data.Text.IO as T

host :: Host
host = Host "127.0.0.1"

port :: Port
port = Port "6800"

main :: IO ()
main = do
  getArgs >>= \case
    ("add":urls) -> do
      uris <- forM urls $ \url -> case parseURI url of
        Nothing -> do
          hPutStrLn stderr $ "Could not parse url: " ++ url
          exitWith (ExitFailure 1)
        Just uri -> return uri
      addUris uris host port >>= \case
        Left err -> T.hPutStrLn stderr err
        Right (GID gid) -> T.putStrLn gid
    _ -> hPutStrLn stderr "Unknown command"
