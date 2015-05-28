{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ByteString.Json where

import Data.ByteString
import Data.Aeson
import qualified Data.Text.Encoding as T
import Control.Monad

instance ToJSON ByteString where
  toJSON = String . T.decodeUtf8

instance FromJSON ByteString where
  parseJSON (String str) = return $ T.encodeUtf8 str
  parseJSON _            = mzero
