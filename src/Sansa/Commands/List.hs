{-# LANGUAGE OverloadedStrings #-}

module Sansa.Commands.List
       ( listCmd ) where

import Sansa.CommandsCommon
import Aria2.Commands (tellActive, tellWaiting, tellStopped)
import Aria2.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>),(<$>))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad

doc :: Doc
doc = text "List downloads."
   <> line <> line
   <> text "If --all is specified, also include already stopped downloads in"
   <> line
   <> text "the output."

listCmd :: Command
listCmd = info (helper <*> listOpts)
            (  fullDesc
            <> headerDoc (Just doc)
            <> progDesc "List downloads"
            )

listOpts :: Parser (CmdAction ())
listOpts = listAction <$>
  flag False True (long "all" <> short 'a' <> help "Include stopped downloads")

listAction :: Bool -> CmdAction ()
listAction stopped = do
  let actions = [ runAria2 tellActive
                  -- this is stupid. There is no 'get all', but only a
                  -- from-to with no indication how much there is...
                  -- let's hope, 99999 is enough
                , runAria2 (tellWaiting 0 99999) ]
               ++ [runAria2 (tellStopped 0 99999) | stopped]
  lst <- concat <$> sequence actions

  liftIO $ T.putStrLn $ T.concat
    [ "GID "
    , padTo (T.length "completed") "Status"
    , "File\n"
    ]

  liftIO $ forM_ lst $ \entry -> do
    let GID gid = diGID entry
    T.putStr (padTo 3 gid)
    T.putStr " "
    T.putStr $ padTo (T.length "complete") $ T.pack $ show (diStatus entry)
    T.putStr " "
    unless (null $ diFiles entry) $
      T.putStr $ T.pack $ fiPath $ head $ diFiles entry
    T.putStrLn ""

padTo :: Int -> Text -> Text
padTo p t
  | T.length t >= p = t
  | otherwise       = T.append t (T.replicate (p - T.length t) " ")
