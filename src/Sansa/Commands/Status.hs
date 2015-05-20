{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Sansa.Commands.Status ( statusCmd ) where

import Sansa.CommandsCommon hiding (empty)
import Aria2.Types
import Aria2.Commands (tellActive, tellWaiting, tellStopped, tellStatus)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Units
import Text.Printf
import Data.Maybe
import Data.List

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
  <$$> fill' (text "Download:")
         <+> progress dl tl
         <+> percent dl tl
         <+> string (showInBest out DataSizeDim dl)
            <+> "/" <+> string (showInBest out DataSizeDim tl)
  <$$> fill' (text "Upload:")
         <+> progress ul tl
         <+> percent ul tl
         <+> string (showInBest out DataSizeDim ul)
            <+> "/" <+> string (showInBest out DataSizeDim tl)
  <$$> fill' (text "Speed:")
         <+> string (showInBest out dataSpeedDim dr) <+> "Down,"
         <+> string (showInBest out dataSpeedDim ur) <+> "Up"
  <$$> fill' (text "ETA:")
         <+> "Down:" <+> eta dl tl dr
         <+> "/" <+> "Up:" <+> eta ul tl ur
  <$$> fill' (text "Directory:")
         <+> (if dir == "" then "(none)" else text dir)
  <$$> text "Files:"
  <$$> indent 2 (vcat $ map (printFile dir) (diFiles di))

  where GID gid = diGID di
        dl = diCompletedLength di
        ul = diUploadLength di
        tl = diTotalLength di

        ur = diUploadSpeed di
        dr = diDownloadSpeed di

        dir = slashify $ fromMaybe "" $ diDir di

        fill' = fill $ maximum (map length keys) + 1
        (keys :: [String]) = ["Download", "Upload", "Speed", "ETA", "Directory"]

        out = printf "%.2g"

printFile :: FilePath -> FileInfo -> Doc
printFile dir fi = text $ fromMaybe path $ stripPrefix dir path
  where path = fiPath fi

percent :: DataSize -> DataSize -> Doc
percent x' total'
  | total == 0 = text "(0%)  "
  | otherwise  = fill (3+1+2) $
                 parens $ int ((x * 100) `div` total) <> text "%"

  where x = floor (x' # Byte)
        total = floor (total' # Byte)

progress :: DataSize -> DataSize -> Doc
progress x' total'
  | total == 0 = brackets $ fill 20 empty
  | otherwise = brackets $ fill 20 $
                   text $ replicate ((x * 20) `div` total) '#'
  where x  = floor (x' # Byte)
        total = floor (total' # Byte)

eta :: DataSize -> DataSize -> DataSpeed -> Doc
eta x total speed
  | (floor (speed # (Byte :/ Second)) :: Int) == 0 = "EOU (End of universe)"
  | otherwise = string $ showTime $ (total |-| x) |/| speed

toDoc :: Show a => a -> Doc
toDoc = text . show

text' :: Text -> Doc
text' = text . T.unpack

slashify :: FilePath -> FilePath
slashify "" = ""
slashify path
  | last path == '/' = path
  | otherwise        = path ++ "/"
