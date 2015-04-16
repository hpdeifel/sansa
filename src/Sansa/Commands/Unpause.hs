module Sansa.Commands.Unpause ( unpauseCmd ) where

import Sansa.CommandsCommon
import Aria2.Types
import Aria2.Commands (unpause, unpauseAll)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Control.Monad

doc :: Doc
doc = string $ unlines
  [ "Resume the downloads specified by the GIDs"
  , ""
  , "If --all is specified, resume all active/waiting downloads."
  , ""
  , "This changes the status of each download from \"paused\" to \"waiting\""
  , "and makes the download eligible to restart."
  ]

unpauseCmd :: Command
unpauseCmd = info (helper <*> unpauseOpts)
             (  fullDesc
             <> headerDoc (Just doc)
             <> progDesc "Resume downloads"
             )

unpauseOpts :: Parser (CmdAction ())
unpauseOpts = unpauseAction <$>
            ( flag' Nothing (  long "all"
                            <> short 'a'
                            <> help "Unpause every active/waiting download")
          <|> (Just <$> some (argument (GID . T.pack <$> str)
                              (metavar "GIDs..."))))

unpauseAction :: Maybe [GID] -> CmdAction ()
unpauseAction  Nothing      = void $ runAria2 unpauseAll
unpauseAction  (Just gids)  = mapM_ pauseOne gids
  where pauseOne gid = void $ runAria2 (unpause gid)
