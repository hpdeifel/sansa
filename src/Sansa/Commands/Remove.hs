module Sansa.Commands.Remove
       ( removeCmd )
       where

import Sansa.CommandsCommon
import Aria2.Commands (remove, forceRemove)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import Control.Monad

doc :: Doc
doc =
    text "Remove the downloads specified by the GIDs"
   <> line <> line
   <> text "This does not remove the downloaded files, but releases resources"
  <+> text "associated with the download."
   <> line
 <$$> text "If the specified download is in progress, it is stopped at first."
  <+> text "The status of a removed download becomes \"removed\"."
   <> line <> line
   <> text "If --force is specified, remove downloads without any action that"
   <> line
   <> text "takes time such as contacting a BitTorrent tracker"

removeCmd :: Command
removeCmd = info (helper <*> removeOpts)
              (  fullDesc
              <> headerDoc (Just doc)
              <> progDesc "Remove downloads"
              )

removeOpts :: Parser (CmdAction ())
removeOpts = removeAction
  <$> flag False True (long "force" <> short 'f' <> help "Force removal")
  <*> some (argument (GID . T.pack <$> str) (metavar "GID..."))

type Force = Bool

removeAction :: Force -> [GID] -> CmdAction ()
removeAction force gids = forM_ gids $ \gid -> do
  let remAction = if force then forceRemove else remove
  void $ runAria2 (remAction gid)
