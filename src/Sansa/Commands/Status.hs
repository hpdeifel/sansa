{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}

module Sansa.Commands.Status ( statusCmd ) where

import Sansa.CommandsCommon hiding (empty)
import Sansa.AsciiStatus
import Aria2.Types
import Aria2.Commands (tellActive, tellWaiting, tellStopped, tellStatus)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Units
import Text.Printf
import Data.Maybe
import Data.List
import Network.URI (uriToString)
import System.IO

doc :: Doc
doc = text "Print the current status of each download"
   <> line <> line
   <> text "If no GID is given, print the status of active/waiting downloads."
   <> line <> line
   <> text "If --all is specified, print the status even for completed downloads"

statusCmd :: Command
statusCmd = info (helper <*> statusOpts)
              (  fullDesc
              <> headerDoc (Just doc)
              <> progDesc "Print status of downloads"
              )

data StatusOpts = Some [GID]
                | All

statusOpts :: Parser (CmdAction ())
statusOpts = statusAction <$>
  (flag' All (long "all" <> short 'a' <> help "Show status of all downloads")
   <|> Some <$> many (argument (GID . T.pack <$> str) (metavar "GID...")))

statusAction :: StatusOpts -> CmdAction ()
statusAction (Some [])   = do
  concat <$> mapM runAria2 [tellActive, tellWaiting 0 99999] >>= \case
    []     -> liftIO $ hPutStrLn stderr "No active downloads."
    active -> printStatus active
statusAction (Some gids) = do
  downloads <- mapM (runAria2 . tellStatus) gids
  printStatus downloads
statusAction All = do
  downloads <- concat <$> mapM runAria2
               [tellActive, tellWaiting 0 99999, tellStopped 0 99999]
  printStatus downloads


printStatus :: [DownloadInfo] -> CmdAction ()
printStatus dis = liftIO $
  putDoc $ vcat (punctuate line $ map printStatus1 dis) <> line

printStatus1 :: DownloadInfo -> Doc
printStatus1 di =
       text "#" <> text' gid <+> toDoc (diStatus di)
  <$$> fill' (text "Download:") <+> downloadLine di
  <$$> fill' (text "Speed:")
         <+> string (showInBest out dataSpeedDim dr)
  <$$> fill' (text "ETA:")
         <+> eta dl tl dr
  <$$> fill' (text "Directory:")
         <+> (if dir == "" then "(none)" else text dir)
  <$$> text "Files:"
  <$$> indent 2 (vcat $ map (printFile dir) (diFiles di))

  where GID gid = diGID di
        dl = diCompletedLength di
        tl = diTotalLength di

        dr = diDownloadSpeed di

        dir = slashify $ fromMaybe "" $ diDir di

        fill' = fill $ maximum (map length keys) + 1
        (keys :: [String]) = ["Download", "Speed", "ETA", "Directory"]

        out = printf "%.2g"

printFile :: FilePath -> FileInfo -> Doc
printFile dir fi = vcat uris <$$> filename
  where path = fiPath fi
        filename = text $ ("-> "++) $ fromMaybe path $ stripPrefix dir path
        uris = map (text . ("<- "++) . printUri) (fiUris fi)
        printUri uri = uriToString id (uiURI uri) ""

toDoc :: Show a => a -> Doc
toDoc = text . show

text' :: Text -> Doc
text' = text . T.unpack

slashify :: FilePath -> FilePath
slashify "" = ""
slashify path
  | last path == '/' = path
  | otherwise        = path ++ "/"
