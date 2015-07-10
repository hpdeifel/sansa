{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Sansa.Commands.Add
       ( addCmd
       ) where

import qualified Data.Text.IO as T

import Sansa.CommandsCommon
import Sansa.Commands.CommonOpts
import Sansa.AsciiStatus
import Aria2.Commands (addUris, tellStatus)
import Aria2.Types

import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import Network.URI
import System.Directory
import Data.Maybe
import Control.Monad
import System.Exit
import Control.Concurrent
import System.IO

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
  <*> switch (long "wait" <> help "Wait for download to finish")

type WaitFinish = Bool

addAction :: DlOptions -> [URI] -> WaitFinish -> CmdAction ()
addAction opts uris wait = do
  cwd <- flip fromMaybe (optDir opts) <$> liftIO getCurrentDirectory
  let opts' = opts { optDir = Just cwd }
  GID gid <- runAria2 $ addUris uris opts'
  liftIO $ do
    T.putStr "Queued download with id: "
    T.putStrLn gid
  when wait $ do
    liftIO $ putStrLn "Waiting for download to finish"
    waitForDownload (GID gid)

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'


-- At the moment, this just polls.
-- A better implementation would use the websocket API of aria2, but that would
-- require some refactoring.
waitForDownload :: GID -> CmdAction ()
waitForDownload gid = do
  di <- runAria2 $ tellStatus gid

  let eta' = eta (diCompletedLength di)
                 (diTotalLength di)
                 (diDownloadSpeed di)

  liftIO $ do
    putDoc $ "\r" <> text (show $ diStatus di) <+> downloadLine di
             <+> "| ETA:" <+> eta'
    hFlush stdout

  case diStatus di of
    StError    -> liftIO $ putStr "\n" >> exitWith (ExitFailure 1)
    StRemoved  -> liftIO $ putStr "\n" >> exitWith (ExitFailure 2)
    StComplete -> liftIO $ putStr "\n" >> exitSuccess
    _          -> do
      liftIO $ threadDelay pollingInverval
      waitForDownload gid

  where pollingInverval = 1 * 1000 * 1000 -- 1 second
