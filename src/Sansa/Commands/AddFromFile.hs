module Sansa.Commands.AddFromFile where

import Sansa.CommandsCommon
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Control.Monad
import Network.URI
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Aria2.Commands (addUris)
import Aria2.Types

doc :: Doc
doc = text "Read URLs from FILE." <> line <> line
   <> text"FILE is read line by line. Every line can contain multiple URLs, which"
   <> line
   <> text "must all point to the same file." <> line <> line
   <> text "FILE is -, read from stdin."

addFromFileCmd :: Command
addFromFileCmd = info (helper <*> affOpts)
                   (  fullDesc
                   <> headerDoc (Just doc)
                   <> progDesc "Read download URLs from file"
                   )

affOpts :: Parser (CmdAction ())
affOpts = affAction <$> argument str (metavar "FILE")

affAction :: FilePath -> CmdAction ()
affAction file = do
  jobs <- lines <$> liftIO (readF file)
  forM_ jobs $ \job -> do
    uris <- mapM parseURI' (words job)
    GID gid <- runAria2 $ addUris uris
    liftIO $ T.putStrLn gid

  where readF "-" = getContents
        readF name = readFile name

parseURI' :: String -> CmdAction URI
parseURI' uri = case parseURI uri of
  Nothing -> lift (throwE $ T.pack $ "Failed to parse URI " ++ uri)
  Just uri' -> return uri'
