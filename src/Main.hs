{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Aria2.Types
import Sansa.CommandsCommon
import Sansa.Commands

import qualified Data.Text as T
import Data.Text (Text)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))

---------------------------------------
-- Command line options and subcommands
---------------------------------------

optParser :: Parser (Options, CmdAction ())
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
          <*> optional (textOption
               (  long "user"
               <> metavar "USERNAME"
               <> help "RPC username for aria2"))
          <*> optional (textOption
               (  long "password"
               <> metavar "PASSWORD"
               <> help "RPC password for aria2"))


commands :: Mod CommandFields (CmdAction ())
commands = command "add" addCmd
        <> command "addfile" addFromFileCmd
        <> command "add-torrent" addTorrentCmd
        <> command "pause" pauseCmd
        <> command "unpause" unpauseCmd
        <> command "remove" removeCmd
        <> command "list" listCmd
        <> command "status" statusCmd

---------------------------------------
-- main
---------------------------------------

doc :: Doc
doc = text "sansa - rpc frontend for aria2" <> line
   <$$> text "To get help for subcommand CMD, call sansa with 'CMD --help' as"
   <$$> text "argument." <> line
   <$$> text "Many commands use or return so called GIDs. These are just"
   <$$> text "download-IDs, that are used to identify specific downloads."

main :: IO ()
main = customExecParser pref opts >>= uncurry runAction

  where opts = info (helper <*> optParser)
               (  fullDesc
               <> headerDoc (Just doc)
               )

        pref = prefs showHelpOnError

textOption ::  Mod OptionFields Text -> Parser Text
textOption = option (T.pack <$> str)
