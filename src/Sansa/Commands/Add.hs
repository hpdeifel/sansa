{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Sansa.Commands.Add
       ( addCmd
       ) where

import qualified Data.Text.IO as T

import Sansa.CommandsCommon
import Sansa.Commands.CommonOpts
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
  <$> commonDlOpts
  <*> some (argument (str >>= readUri) (metavar "URL..."))

addAction :: DlOptions -> [URI] -> CmdAction ()
addAction opts uris = do
  cwd <- flip fromMaybe (optDir opts) <$> liftIO getCurrentDirectory
  let opts' = opts { optDir = Just cwd }
  GID gid <- runAria2 $ addUris uris opts'
  liftIO $ do
    T.putStr "Queued download with id: "
    T.putStrLn gid

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'
