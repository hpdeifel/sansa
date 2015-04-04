{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Json where

import Network.URI
import Data.Aeson
import qualified Data.Text as T
import Control.Monad

instance ToJSON URI where
  toJSON uri = String $ T.pack $ uriToString id uri ""

instance FromJSON URI where
  parseJSON (String str) = case parseURI (T.unpack str) of
    Nothing  -> mzero
    Just uri -> return uri
  parseJSON _ = mzero
