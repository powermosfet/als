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

url :: [Char] -> ExceptT Error IO Http.Request
url path = "https://a.wunderlist.com/api/v1/" ++ path
    & Http.parseRequest
    & withExceptT HttpException

data Error =
    HttpException Http.HttpException
    deriving (Show)

getLists :: Auth -> ExceptT Error IO [List.List]
getLists auth = do
    req <- url "lists"
        <&> withAuth auth
    response <- liftIO $ Http.httpJSON req
    return (Http.getResponseBody response)

createTask :: Auth -> CreateTask.CreateTask -> ExceptT Error IO Task.Task
createTask auth payload = do
    req <- url "tasks"
        <&> withAuth auth
        <&> Http.setRequestMethod "POST"
        <&> Http.setRequestBodyJSON payload
    response <- liftIO $ Http.httpJSON req
    return (Http.getResponseBody response)
