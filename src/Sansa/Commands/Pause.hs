module Sansa.Commands.Pause
       ( pauseCmd
       ) where

import Sansa.CommandsCommon
import Aria2.Types
import Aria2.Commands (pause, pauseAll)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))

doc :: Doc
doc = string $ unlines
  [ "Pause the downloads specified by the GIDs"
  , ""
  , "If --all is specified, pause all active/waiting downloads."
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

pauseOpts :: Parser (CmdAction ())
pauseOpts = pauseAction <$>
            ( flag' Nothing (  long "all"
                            <> short 'a'
                            <> help "Pause every active/waiting download")
          <|> (Just <$> some (argument (GID . T.pack <$> str)
                              (metavar "GIDs..."))))

pauseAction :: Maybe [GID] -> CmdAction ()
pauseAction  Nothing      = void $ runAria2 pauseAll
pauseAction  (Just gids)  = mapM_ pauseOne gids
  where pauseOne gid = runAria2 (pause gid) >>= \(GID gid') ->
          liftIO $ T.putStrLn gid'
