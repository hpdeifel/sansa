{-# LANGUAGE LambdaCase #-}
module Sansa.Commands.Add
       ( addCmd
       ) where

import qualified Data.Text.IO as T

import Sansa.CommandsCommon
import Aria2.Commands (addUris)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Network.URI
import System.Directory
import Data.Maybe

doc :: Doc
doc = text "Add URLs pointing to a single file for download." <> line
   <$$> text "By default, the downloaded files are saved to the current directory."
   <$$> text "This can be overwritten with --dir." <> line
   <$$> text "Returns the GID of the download."

addCmd :: Command
addCmd = info (helper <*> addOpts)
           (  fullDesc
           <> headerDoc (Just doc)
           <> progDesc "Add URLs for download"
           )

addOpts :: Parser (CmdAction ())
addOpts = addAction
  <$> optional (strOption
       ( long "dir"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Directory to save the files to"))
  <*> some (argument (str >>= readUri) (metavar "URL..."))

addAction :: Maybe FilePath -> [URI] -> CmdAction ()
addAction dir uris = do
  cwd <- flip fromMaybe dir <$> liftIO getCurrentDirectory
  let opts = DlOptions {
        optDir = Just cwd
      }
  GID gid <- runAria2 $ addUris uris opts
  liftIO $ T.putStrLn gid

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'
