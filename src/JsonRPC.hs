{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module JsonRPC

       ( Request(..)
       , Response(..)
       , Credentials(..)
       , sendRequest
       , sendRequest'
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Network.URI
import qualified Network.HTTP.Base as HTTP
import Network.HTTP hiding (Response, Request, user)
import Network.Browser
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock.POSIX
import Data.Maybe
import System.IO

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

data Credentials = Credentials Text Text

sendRequest :: URI -> (Maybe Credentials) -> Request -> IO (Either Text Response)
sendRequest uri cred req =
  browse browser >>= \case
    (_, res) -> return $ parseResponse res

  where httpRequest = HTTP.Request {
          rqURI = uri,
          rqMethod = POST,
          rqHeaders =
            [ Header HdrContentType "application/json-rpc; charset=utf-8"
            , Header HdrContentLength (show $ B.length body)],
          rqBody = body
        }

        browser = do
          -- FIXME: Browser still prints some absolutely unusable error messages
          setErrHandler (const $ return ())
          setDebugLog Nothing
          setOutHandler (const $ return ())
          setAuthorityGen $ \_ _ -> case cred of
            Nothing -> do
              liftIO $ hPutStrLn stderr "Authentication requested, but no credentials given"
              return Nothing
            Just (Credentials user pw) -> return $ Just (T.unpack user, T.unpack pw)
          request httpRequest

        body = encode req

        parseResponse :: HTTP.Response ByteString -> Either Text Response
        parseResponse res = either (Left . T.pack) Right $
                              eitherDecode (rspBody res)

-- | Like sendRequest, but fills the id with something a little bit random
sendRequest' :: URI -> (Maybe Credentials) -> Request -> IO (Either Text Response)
sendRequest' uri cred req = do
  time <- getPOSIXTime
  sendRequest uri cred req { requestId = T.pack (show time) }
