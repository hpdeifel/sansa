module Sansa.Commands.Options
       ( optsCmd
       ) where

import Sansa.CommandsCommon
import Sansa.Commands.CommonOpts
import Aria2.Types
import Aria2.Commands (getGlobalOption, getOption)

import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson

import Control.Monad

doc :: Doc
doc = text "Get or set options."

optsCmd :: Command
optsCmd = info (helper <*> optsOpts)
            (  fullDesc
            <> headerDoc (Just doc)
            <> progDesc "Get or set options"
            )

optsOpts :: Parser (CmdAction ())
optsOpts = subparser $ command "get" getCmd
                    <> command "set" setCmd

------------------
-- Get -----------
------------------

getDoc :: Doc
getDoc =  text "Print current options to stdout." <> line
     <$$> text "If --global is specified, output global options."
      <+> text "Otherwise, the options for the download denoted by GID"
      <+> text "Will be printed"

getCmd :: Command
getCmd = info (helper <*> getOpts)
           (  fullDesc
           <> headerDoc (Just getDoc)
           <> progDesc "Get current settings"
           )

getOpts :: Parser (CmdAction ())
getOpts = getAction <$>
  (   flag' Nothing (long "global" <> help "Print global options")
  <|> Just <$> argument (GID . T.pack <$> str) (metavar "GID")
  )

getAction :: Maybe GID -> CmdAction ()
getAction Nothing     = runAria2 getGlobalOption >>= liftIO . printOptions
getAction (Just gid)  = runAria2 (getOption gid) >>= liftIO . printOptions

printOptions :: Object -> IO ()
printOptions opts = forM_ (M.toList opts) $ \(key,val) -> do
    T.putStr key
    putStr ": "
    B.putStrLn $ encode val


------------------
-- Set -----------
------------------

setDoc :: Doc
setDoc =  text "Change download options" <> line
     <$$> text "If --global is specified, change the default options."
      <+> text "Otherwise, the options for one specific download are changed."

setCmd :: Command
setCmd = info (helper <*> setOpts)
           (  fullDesc
           <> headerDoc (Just setDoc)
           <> progDesc "Set download options"
           )

setOpts :: Parser (CmdAction ())
setOpts = setAction <$>
  (   flag' () (long "global" <> help "Change default options")
      *> (Left <$> commonGROpts)
  <|> Right <$> argument (GID . T.pack <$> str) (metavar "GID"))

setAction :: Either GlobalRuntimeOptions GID -> CmdAction ()
setAction (Left opts) = error "not implemented yet"
setAction (Right gid)  = error "not implemented yet"
