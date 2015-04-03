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

doc :: Doc
doc = text "Add URLs pointing to a single file for download."
   <> line <> line
   <> text "Returns the GID of the download."

addCmd :: Command
addCmd = info (helper <*> addOpts)
           (  fullDesc
           <> headerDoc (Just doc)
           <> progDesc "Add URLs for download"
           )

addOpts :: Parser (CmdAction ())
addOpts = addAction <$> some (argument (str >>= readUri) (metavar "URL..."))

addAction :: [URI] -> CmdAction ()
addAction uris = do
  GID gid <- runAria2 $ addUris uris
  liftIO $ T.putStrLn gid

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'
