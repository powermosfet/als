{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Outlook.Request
    ( Response(..)
    , OutlookApiErrorDetails(..)
    , Error(..)
    , unwrap
    , send
    , withToken
    ) where

import Protolude

import Control.Monad (fail)
import Data.Aeson (ToJSON, FromJSON, parseJSON, Value(..))
import GHC.Generics (Generic)
import qualified Config
import qualified Data.HashMap.Lazy as HashMap
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Header as HttpHeader
import qualified Network.HTTP.Types.Status as Status

data Error
  = ConfigError Config.Error
  | JsonExceptionError Http.JSONException
  | HttpStatusError Status.Status [(HttpHeader.HeaderName, ByteString)]
  | OutlookApiError OutlookApiErrorDetails
  deriving (Show)

data Response a 
    = SuccessResponse (OutlookApiSuccess a)
    | ErrorResponse OutlookApiErrorDetails
    deriving (Show)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (value@(Object v)) =
    let
        entry = head (HashMap.toList v)
    in
    case entry of
        Just ("error", _) -> ErrorResponse <$> parseJSON value
        _ -> SuccessResponse <$> parseJSON value

  parseJSON _ = fail "Invalid JSON type"

data OutlookApiSuccess a = OutlookApiSuccess
    { value :: a
    } deriving (Show, Generic, FromJSON)

data OutlookApiErrorDetails = OutlookApiErrorDetails
    { error :: InnerOutlookError
    } deriving (Show, Generic, FromJSON)

data InnerOutlookError = InnerOutlookError
    { code :: [Char]
    , message :: [Char]
    } deriving (Show, Generic, FromJSON)

unwrap :: Response a -> ExceptT Error IO a
unwrap response =
  case response of
    SuccessResponse (OutlookApiSuccess a) -> return a
    ErrorResponse errorDetails -> throwError (OutlookApiError errorDetails)
    

withToken :: Http.Request -> ExceptT Error IO Http.Request
withToken req = do
  accessToken <- withExceptT ConfigError $ Config.findOption "ACCESS_TOKEN" Config.bytestring
  return $ Http.addRequestHeader "Authorization" ("Bearer " <> accessToken) req

send :: (FromJSON a) => Http.Request -> ExceptT Error IO a
send req = do
  response <- liftIO $ Http.httpJSONEither req
  let status = Http.getResponseStatus response
  let body = Http.getResponseBody response
  if status == Status.created201 || status == Status.ok200 then
    ExceptT (return (Http.getResponseBody response))
      & withExceptT JsonExceptionError
  else
    throwError (HttpStatusError status (Http.getResponseHeaders response))
