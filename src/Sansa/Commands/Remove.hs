module Sansa.Commands.Remove
       ( removeCmd )
       where

import Sansa.CommandsCommon
import Aria2.Commands (remove)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad

doc :: Doc
doc =
    text "Remove the downloads specified by the GIDs"
   <> line <> line
   <> text "If the specified download is in progress, it is stopped at first."
   <> line
   <> text "The status of a removed download becomes \"removed\"."
   <> line
   <> text "Prints the GID for every download"

removeCmd :: Command
removeCmd = info (helper <*> removeOpts)
              (  fullDesc
              <> headerDoc (Just doc)
              <> progDesc "Remove downloads"
              )

removeOpts :: Parser (CmdAction ())
removeOpts = removeAction <$> some (argument (GID . T.pack <$> str)
                                    (metavar "GID..."))

removeAction :: [GID] -> CmdAction ()
removeAction gids = forM_ gids $ \gid -> do
  GID gid' <- runAria2 (remove gid)
  liftIO $ T.putStrLn gid'
