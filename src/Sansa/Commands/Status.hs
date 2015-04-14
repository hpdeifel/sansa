{-# LANGUAGE OverloadedStrings #-}

module Sansa.Commands.Status ( statusCmd ) where

import Sansa.CommandsCommon
import Aria2.Types
import Aria2.Commands (tellActive, tellWaiting, tellStopped, tellStatus)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Monad
import Data.Units
import Text.Printf

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
  active <- concat <$> mapM runAria2 [tellActive, tellWaiting 0 99999]
  printStatus active
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
  <$$> text "Download:"
         <+> progress dl tl
         <+> percent dl tl
         <+> string (showInBest out dl) <+> "/" <+> string (showInBest out tl)
  <$$> fill (T.length "Download:") (text "Upload:")
         <+> progress ul tl
         <+> percent ul tl
         <+> string (showInBest out ul) <+> "/" <+> string (showInBest out tl)
  <$$> fill (T.length "Download:") (text "Speed")
         <+> int dr <+> "B/s" <+> "Down,"
         <+> int ur <+> "B/s" <+> "Up"
  <$$> text "Files:"
  <$$> indent 2 (vcat $ map printFile (diFiles di))

  where GID gid = diGID di
        dl = diCompletedLength di
        ul = diUploadLength di
        tl = diTotalLength di

        ur = diUploadSpeed di
        dr = diDownloadSpeed di

        out = printf "%.2g"

printFile :: FileInfo -> Doc
printFile = text . fiPath

percent :: DataSize -> DataSize -> Doc
percent x' total' = fill (3+1+2) $
  parens $ int ((x * 100) `div` total) <> text "%"

  where x = floor (x' # Byte)
        total = floor (total' # Byte)

progress :: DataSize -> DataSize -> Doc
progress x' total' = brackets $ fill 20 $
                   text $ replicate ((x * 20) `div` total) '#'
  where x  = floor (x' # Byte)
        total = floor (total' # Byte)

toDoc :: Show a => a -> Doc
toDoc = text . show

text' :: Text -> Doc
text' = text . T.unpack
