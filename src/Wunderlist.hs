{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wunderlist
    ( Auth(..)
    , GetListsError(..)
    , getLists
    , List(..)
    ) where

import Protolude

import Data.ByteString as B
import qualified Network.HTTP.Simple as Http
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Auth = Auth
    { clientId :: B.ByteString
    , accessToken :: B.ByteString
    }

withAuth :: Auth -> Http.Request -> Http.Request
withAuth (Auth {..}) req =
    req
        & Http.addRequestHeader "X-Client-ID" clientId
        & Http.addRequestHeader "X-Access-Token" accessToken

data List = List
    { id :: Int
    , created_at :: Text
    , title :: Text
    -- , listType :: Text
    -- , type_ :: Text
    , revision :: Int
    } deriving (Show, Generic, FromJSON)

data GetListsError =
    UrlParseError [Char]
    deriving (Show)

getLists :: Auth -> ExceptT GetListsError IO [List]
getLists auth = do
    req <- Http.parseRequest "https://a.wunderlist.com/api/v1/lists"
        & withExceptT UrlParseError
        <&> withAuth auth
    response <- liftIO $ Http.httpJSON req
    return (Http.getResponseBody response)
