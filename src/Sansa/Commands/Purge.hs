module Sansa.Commands.Purge
       ( purgeCmd
       ) where

import Sansa.CommandsCommon
import Aria2.Commands (purgeDownloadResult, removeDownloadResult)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Control.Monad

doc :: Doc
doc =   text "Purge download information from the download list." <> line
   <$$> text "This can only be used on stopped downloads to release any"
    <+> text "resources associated with the download"

purgeCmd :: Command
purgeCmd = info (helper <*> purgeOpts)
             (  fullDesc
             <> headerDoc (Just doc)
             <> progDesc "Purge download information"
             )

purgeOpts :: Parser (CmdAction ())
purgeOpts = purgeAction
  <$> (flag' Nothing (long "all" <> short 'a' <> help "Purge all downloads")
       <|> Just <$> some (argument (GID . T.pack <$> str) (metavar "GID...")))

purgeAction :: Maybe [GID] -> CmdAction ()
purgeAction Nothing     = void $ runAria2 purgeDownloadResult
purgeAction (Just gids) = forM_ gids $ \gid ->
  runAria2 $ removeDownloadResult gid
