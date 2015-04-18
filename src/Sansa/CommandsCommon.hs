{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Sansa.CommandsCommon
       ( module Options.Applicative
       , module Control.Monad.IO.Class
       , Options(..)
       , CmdAction
       , Command
       , runAction
       , runAria2
       ) where

import Aria2.Types
import qualified Aria2.Commands as AC
import Options.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import System.Exit

data Options = Options {
  host :: Host,
  port :: Port,
  user :: Maybe Text,
  password :: Maybe Text
}

type CmdAction = ReaderT Options (ExceptT Text IO)

type Command = ParserInfo (CmdAction ())

runAction :: Options -> CmdAction () -> IO ()
runAction opts act = runExceptT (runReaderT act opts) >>= \case
  Left err -> T.hPutStrLn stderr (T.append "error: " err)
              >> exitWith (ExitFailure 1)
  Right x -> return x

runAria2 :: AC.Command a -> CmdAction a
runAria2 act = do
  conSettings <- AC.ConSettings <$> asks host <*> asks port
                                <*> asks user <*> asks password
  liftIO (AC.runCommand conSettings act) >>= either (lift.throwE) return
