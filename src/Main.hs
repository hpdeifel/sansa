{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Aria2.Types
import Sansa.Commands

import qualified Data.Text as T
import Data.Text (Text)
import Options.Applicative

---------------------------------------
-- Command line options and subcommands
---------------------------------------

optParser :: Parser (Options, CmdAction)
optParser = (,) <$> opts <*> subparser commands
  where opts = Options
          <$> (Host <$> textOption
               (  long "host"
               <> metavar "HOSTNAME"
               <> value "localhost"
               <> help "Host where the aria2-rpc server runs"))
          <*> (Port <$> textOption
               (  long "port"
               <> short 'p'
               <> metavar "PORT"
               <> value "6800"
               <> help "Port of the aria2-rpc server"))


commands :: Mod CommandFields CmdAction
commands = command "add" (info addCommand fullDesc)
        <> command "foo" (info addCommand fullDesc)


---------------------------------------
-- main
---------------------------------------

main :: IO ()
main = execParser opts >>= \(os, cmd) -> cmd os

  where opts = info (helper <*> optParser)
               (  fullDesc
               <> header "sansa - rpc frontend for aria2"
               )

textOption ::  Mod OptionFields Text -> Parser Text
textOption = option (T.pack <$> str)
