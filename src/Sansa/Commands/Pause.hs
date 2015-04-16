module Sansa.Commands.Pause
       ( pauseCmd
       ) where

import Sansa.CommandsCommon
import Aria2.Types
import Aria2.Commands (pause, forcePause, pauseAll, forcePauseAll)
import Control.Monad
import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))

doc :: Doc
doc = string $ unlines
  [ "Pause the downloads specified by the GIDs"
  , ""
  , "If --all is specified, pause all active/waiting downloads."
  , ""
  , "If --force is specified, pause the download without any action"
  , "that takes time such as contacting a BitTorrent tracker."
  , ""
  , "The status of paused downloads becomes \"paused\". If the download"
  , "is active, it is placed on the first position of the waiting queue."
  , "As long as the status is \"paused\", it will not be started. To change"
  , "the status to \"waiting\", use the 'unpause' command."
  ]

pauseCmd :: Command
pauseCmd = info (helper <*> pauseOpts)
             (  fullDesc
             <> headerDoc (Just doc)
             <> progDesc "Pause downloads"
             )

type Force = Bool

pauseOpts :: Parser (CmdAction ())
pauseOpts = pauseAction
   <$> flag False True (long "force" <> short 'f' <> help "Force the pause")
   <*> (    flag' Nothing ( long "all" <> short 'a'
                         <> help "Pause every active/waiting download")
        <|> (Just <$> some (argument (GID . T.pack <$> str)
                            (metavar "GIDs..."))))

pauseAction :: Force -> Maybe [GID] -> CmdAction ()
pauseAction False  Nothing = void $ runAria2 pauseAll
pauseAction True  Nothing = void $ runAria2 forcePauseAll
pauseAction False (Just gids)  = mapM_ pauseOne gids
  where pauseOne gid = void $ runAria2 (pause gid)
pauseAction True (Just gids)  = mapM_ pauseOne gids
  where pauseOne gid = void $ runAria2 (forcePause gid)
