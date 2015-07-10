{-# LANGUAGE OverloadedStrings #-}
module Sansa.AsciiStatus
       ( progressBar
       , percent
       , eta
       , downloadLine
       ) where

import Aria2.Types
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc,(<+>))
import Data.Units
import Data.Monoid
import Text.Printf

progressBar :: DataSize -> DataSize -> Doc
progressBar x' total'
  | total == 0 = Doc.brackets $ Doc.fill width Doc.empty
  | otherwise = Doc.brackets $ Doc.fill width $
                   Doc.text $ replicate ((x * width) `div` total) fullChar
  where x  = floor (x' # Byte)
        total = floor (total' # Byte)

        width = 20
        fullChar = '#'


percent :: DataSize -> DataSize -> Doc
percent x' total'
  | total == 0 = Doc.text "(0%)  "
  | otherwise  = Doc.fill (3+1+2) $
                 Doc.parens $ Doc.int ((x * 100) `div` total) <> Doc.text "%"

  where x = floor (x' # Byte)
        total = floor (total' # Byte)

eta :: DataSize -> DataSize -> DataSpeed -> Doc
eta x total speed
  | (floor (speed # (Byte :/ Second)) :: Int) == 0 = "EOU (End of universe)"
  | otherwise = Doc.string $ showTime $ (total |-| x) |/| speed


downloadLine :: DownloadInfo -> Doc
downloadLine di = progressBar dl tl <+> percent dl tl <+> dlStr <+> "/" <+> tlStr
  where dl = diCompletedLength di
        tl = diTotalLength di

        dlStr = Doc.string (showInBest out DataSizeDim dl)
        tlStr = Doc.string (showInBest out DataSizeDim tl)

        out = printf "%.2g"
