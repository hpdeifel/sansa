{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Sansa.AsciiStatus
       ( progressBar
       , percent
       , eta
       , downloadLine
       , OverwriteT
       , withOverwrite
       , overwrite
       , overwriteDoc
       ) where

import Aria2.Types
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc,(<+>))
import Data.Units
import Data.Monoid
import Text.Printf
import Control.Monad.State
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

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


newtype OverwriteT m a = OverwriteT { runOverwriteT :: StateT Int m a }
  deriving (Monad, Applicative, Functor, MonadIO)

instance MonadTrans OverwriteT where
  lift = OverwriteT . lift

withOverwrite :: (Applicative m, MonadIO m) => OverwriteT m a -> m a
withOverwrite action = evalStateT (runOverwriteT action) 0
                       <* liftIO (T.putStr "\n")

overwrite :: MonadIO m => Text -> OverwriteT m ()
overwrite message = do
  oldLen <- OverwriteT get

  let paddedMsg = T.justifyLeft oldLen ' ' message

  liftIO $ T.putStr ("\r" <> paddedMsg) >> hFlush stdout
  OverwriteT $ put (T.length message)

overwriteDoc :: MonadIO m => Doc -> OverwriteT m ()
overwriteDoc = overwrite . T.pack . (`Doc.displayS` "") . Doc.renderPretty 0.4 80
