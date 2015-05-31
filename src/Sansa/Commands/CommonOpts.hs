module Sansa.Commands.CommonOpts where

import Aria2.Types
import Sansa.CommandsCommon

commonDlOpts :: Parser DlOptions
commonDlOpts = DlOptions
  <$> optional (strOption
       ( long "dir"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Directory to save the files to"))
  <*> optional (strOption
       ( long "out"
      <> short 'o'
      <> metavar "FILENAME"
      <> help "Basename of the saved file"))

  <*> (option readFollow
       ( long "follow-torrent"
      <> help "Follow torrent urls. Can be true, false or mem"
      <> value Follow
      <> showDefaultWith printFollow
       ))

  <*> (Wrap <$> switch
       (  long "pause"
       <> help "Immediately pause download"
       ))

readFollow :: ReadM FollowOption
readFollow = eitherReader $ \s -> case s of
  "true"  -> return Follow
  "false" -> return DontFollow
  "mem"   -> return FollowMem
  _       -> Left $
     "Unknown input \"" ++ s ++ "\". Expected true, false or mem"

printFollow :: FollowOption -> String
printFollow Follow = "true"
printFollow DontFollow = "false"
printFollow FollowMem = "mem"

-- Global runtime options

commonGROpts :: Parser GlobalRuntimeOptions
commonGROpts = GROptions
  <$> optional (option auto
       ( long "max-concurrent-downloads"
      <> metavar "NUM"
      <> help "Maximum number of parallel downloads"
       ))
