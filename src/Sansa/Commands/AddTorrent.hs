module Sansa.Commands.AddTorrent
       ( addTorrentCmd
       ) where

import Sansa.CommandsCommon
import Sansa.Commands.CommonOpts
import Aria2.Commands (addTorrent)
import Aria2.Types

import System.Directory
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))

doc :: Doc
doc = text "Add a local \".torrent\" file for download" <> line
   <$$> text "If you want to add a torrent file from a remote url, see"
   <+> text "'sansa add' and its --follow-torrent option." <> line
   <$$> text "If - is used instead of the filename, read the torrent from stdin."

addTorrentCmd :: Command
addTorrentCmd = info (helper <*> addTorrentOpts)
                  (  fullDesc
                  <> headerDoc (Just doc)
                  <> progDesc "Add local .torrent file"
                  )

addTorrentOpts :: Parser (CmdAction ())
addTorrentOpts = addTAction <$> commonDlOpts <*> strArgument (metavar "FILE")

addTAction :: DlOptions -> FilePath -> CmdAction ()
addTAction opts path = do
  file <- liftIO $ readF path
  cwd <- flip fromMaybe (optDir opts) <$> liftIO getCurrentDirectory
  let opts' = opts { optDir = Just cwd }

  GID gid <- runAria2 $ addTorrent (Base64.encode file) [] opts'

  liftIO $ putStrLn $ "Queued download with id: " ++ show gid

  where readF "-"  = B.getContents
        readF name = B.readFile name
