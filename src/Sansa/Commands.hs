module Sansa.Commands where

import Aria2.Types

import Options.Applicative

-- TODO Remove
import Network.URI

data Options = Options {
  host :: Host,
  port :: Port
}

type CmdAction = Options -> IO ()

type Command = ParserInfo CmdAction

readUri :: String -> ReadM URI
readUri uri = case parseURI uri of
  Nothing -> readerError $ "Could not parse URI " ++ show uri
  Just uri' -> return uri'

addCommand :: Parser (Options -> IO ())
addCommand = const (\_ -> putStrLn "add") <$>
  (some $ argument (str >>= readUri) (metavar "URL.."))
