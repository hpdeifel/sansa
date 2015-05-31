module Sansa.Commands.Stop
       ( stopCmd )
       where

import Sansa.CommandsCommon
import Aria2.Commands (remove, forceRemove)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Control.Monad

doc :: Doc
doc =
    text "Stop the downloads specified by the GIDs" <> line
 <$$> text "This action is not reversible. To temporarily pause downloads, see"
  <+> text "'pause'." <> line
 <$$> text "The status of a stopped download becomes \"removed\"."
   <> line <> line
   <> text "If --force is specified, stop downloads without any action that"
   <> line
   <> text "takes time such as contacting a BitTorrent tracker"

stopCmd :: Command
stopCmd = info (helper <*> stopOpts)
              (  fullDesc
              <> headerDoc (Just doc)
              <> progDesc "Stop downloads"
              )

stopOpts :: Parser (CmdAction ())
stopOpts = stopAction
  <$> flag False True (long "force" <> short 'f' <> help "Force removal")
  <*> some (argument (GID . T.pack <$> str) (metavar "GID..."))

type Force = Bool

stopAction :: Force -> [GID] -> CmdAction ()
stopAction force gids = forM_ gids $ \gid -> do
  let remAction = if force then forceRemove else remove
  void $ runAria2 (remAction gid)
