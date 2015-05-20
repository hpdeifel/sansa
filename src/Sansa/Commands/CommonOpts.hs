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
