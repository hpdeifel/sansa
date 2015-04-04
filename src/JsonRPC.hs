{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module JsonRPC

       ( Request(..)
       , Response(..)
       , sendRequest
       , sendRequest'
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Control.Applicative
import Control.Monad
import Network.URI
import qualified Network.HTTP.Base as HTTP
import Network.HTTP hiding (Response, Request)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock.POSIX
import Data.Maybe

-- Requests

data Request = Request {
  method    :: Text,
  arguments :: [Value],
  requestId :: Text
} deriving (Show)

instance ToJSON Request where
  toJSON req = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= method req
    , "params"  .= arguments req
    , "id"      .= requestId req
    ]

instance FromJSON Request where
  parseJSON (Object v) = Request
    <$> v .: "method"
    <*> v .: "params"
    <*> v .: "id"
  parseJSON _ = mzero

-- Responses

data Response = Response {
  result     :: Value,
  errorObj   :: Value,
  responseId :: Text
} deriving (Show)

instance ToJSON Response where
  toJSON res = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "result"  .= result res
    , "error"   .= errorObj res
    , "id"      .= responseId res
    ]

instance FromJSON Response where
  parseJSON (Object v) = Response
    <$> (fromMaybe Null <$> v .:? "result")
    <*> (fromMaybe Null <$> v .:? "error")
    <*> v .: "id"
  parseJSON _ = mzero

sendRequest :: URI -> Request -> IO (Either Text Response)
sendRequest uri req =
  simpleHTTP httpRequest >>= \case
    Left err -> return $ Left $ T.pack $ show err
    Right res -> return $ parseResponse res

  where httpRequest = HTTP.Request {
          rqURI = uri,
          rqMethod = POST,
          rqHeaders =
            [ Header HdrContentType "application/json-rpc; charset=utf-8"
            , Header HdrContentLength (show $ B.length body)],
          rqBody = body
        }

        body = encode req

        parseResponse :: HTTP.Response ByteString -> Either Text Response
        parseResponse res = either (Left . T.pack) Right $
                              eitherDecode (rspBody res)

-- | Like sendRequest, but fills the id with something a little bit random
sendRequest' :: URI -> Request -> IO (Either Text Response)
sendRequest' uri req = do
  time <- getPOSIXTime
  sendRequest uri req { requestId = T.pack (show time) }
