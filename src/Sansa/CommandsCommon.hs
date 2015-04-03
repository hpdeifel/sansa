module Sansa.CommandsCommon
       ( module Options.Applicative
       , Options(..)
       , CmdAction
       , Command
       ) where

import Aria2.Types
import Options.Applicative

data Options = Options {
  host :: Host,
  port :: Port
}

type CmdAction = Options -> IO ()

type Command = ParserInfo CmdAction
