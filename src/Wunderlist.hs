{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wunderlist
    ( Auth(..)
    , Error(..)
    , createTask
    , getLists
    ) where

import Protolude

import Data.ByteString as B
import qualified Network.HTTP.Simple as Http
import qualified Wunderlist.CreateTask as CreateTask
import qualified Wunderlist.List as List
import qualified Wunderlist.Task as Task

data Auth = Auth
    { clientId :: B.ByteString
    , accessToken :: B.ByteString
    } deriving (Show)

withAuth :: Auth -> Http.Request -> Http.Request
withAuth (Auth {..}) req =
    req
        & Http.addRequestHeader "X-Client-ID" clientId
        & Http.addRequestHeader "X-Access-Token" accessToken

url :: [Char] -> [Char]
url path = "https://a.wunderlist.com/api/v1/" ++ path

data Error =
    UrlParseError [Char]
    deriving (Show)

getLists :: Auth -> ExceptT Error IO [List.List]
getLists auth = do
    req <- Http.parseRequest (url "lists")
        & withExceptT UrlParseError
        <&> withAuth auth
    response <- liftIO $ Http.httpJSON req
    return (Http.getResponseBody response)

createTask :: Auth -> CreateTask.CreateTask -> ExceptT Error IO Task.Task
createTask auth payload = do
    req <- Http.parseRequest (url "tasks")
        & withExceptT UrlParseError
        <&> withAuth auth
        <&> Http.setRequestMethod "POST"
        <&> Http.setRequestBodyJSON payload
    response <- liftIO $ Http.httpJSON req
    return (Http.getResponseBody response)
