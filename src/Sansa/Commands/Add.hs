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
import Control.Monad.Trans.Class
import System.Exit
import Control.Concurrent

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
    waitForDownload (GID gid) >>= liftIO . exitWith

readUri :: String -> ReadM URI
readUri uri = case parseURI $ escapeSpaces uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'

  -- Allow the input URI to contain spaces
  where escapeSpaces = escapeURIString (/= ' ')


-- At the moment, this just polls.
-- A better implementation would use the websocket API of aria2, but that would
-- require some refactoring.
waitForDownload :: GID -> CmdAction ExitCode
waitForDownload gid = withOverwrite loop

  where loop = do
          di <- lift $ runAria2 $ tellStatus gid

          let eta' = eta (diCompletedLength di)
                         (diTotalLength di)
                         (diDownloadSpeed di)

          overwriteDoc $ text (show $ diStatus di) <+> downloadLine di
                     <+> "| ETA:" <+> eta'

          case diStatus di of
            StError    -> return (ExitFailure 1)
            StRemoved  -> return (ExitFailure 2)
            StComplete -> return ExitSuccess
            _          -> do
              liftIO $ threadDelay pollingInverval
              loop

        pollingInverval = 1 * 1000 * 1000 -- 1 second
