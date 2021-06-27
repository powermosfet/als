{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Outlook
    ( Error(..)
    , createTask
    , getLists
    ) where

import Protolude

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Char8 as B
import Outlook.CreateTask (CreateTask)
import Outlook.Task (Task)
import System.Environment (setEnv)
import qualified Config
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Status as Status
import qualified Outlook.Auth as Auth
import qualified Outlook.AuthToken as AuthToken
import qualified Outlook.CreateTask as CreateTask 
import qualified Outlook.List as List 
import qualified Outlook.Request as Request
import qualified Outlook.Task as Task 
import qualified Data.Text as T

httpWithReauth :: (FromJSON a) => Http.Request -> ExceptT Error IO a
httpWithReauth req = do
    catchE (req & Request.withToken >>= Request.send & withExceptT OutlookResponseError)
      (\err ->
        case err of
          OutlookResponseError (Request.HttpStatusError status headers) ->
            if status == Status.unauthorized401 then do
                print "Unauthorized... :'( Fetching new token"
                newAccessToken <- encodeUtf8 <$> AuthToken.access_token <$> withExceptT AuthError Auth.refresh
                print "Got new token!"
                liftIO $ setEnv "ACCESS_TOKEN" $ T.unpack $ decodeUtf8 newAccessToken
                req & Request.withToken >>= Request.send & withExceptT OutlookResponseError
            else
                throwError (OutlookResponseError (Request.HttpStatusError status headers))
          _ -> throwError err
      )
    
listListUrl :: [Char]
listListUrl = "https://graph.microsoft.com/v1.0/me/todo/lists" 

listUrl :: List.Id -> [Char]
listUrl listId = listListUrl <> "/" <> listId <> "/tasks"

taskUrl :: List.Id -> Task.Id -> [Char]
taskUrl listId taskId = listUrl listId <> "/" <> taskId

request :: [Char] -> ExceptT Error IO Http.Request
request url = url
    & Http.parseRequest
    & withExceptT HttpException

data Error
    = ConfigError Config.Error
    | HttpException Http.HttpException
    | AuthError Auth.Error
    | OutlookResponseError Request.Error
    deriving (Show)

findOption :: [Char] -> ([Char] -> ExceptT Config.Error IO a) -> ExceptT Error IO a
findOption name convert = 
    Config.findOption name convert
        &   withExceptT ConfigError

findExisting :: CreateTask -> [ Task ] -> Maybe Task
findExisting inputTask existingTasks =
    Protolude.find (Task.matchesTitle (CreateTask.title inputTask)) existingTasks

createOrUpdateTask :: CreateTask -> ExceptT Error IO Task
createOrUpdateTask payload = do
    print "createOrUpdateTask"
    allTasks <- getTasks
    let existingTask = findExisting payload allTasks
    case existingTask of
      Just someTask -> Task.increment someTask & updateTask
      Nothing -> createTask payload

createTask :: CreateTask -> ExceptT Error IO Task
createTask payload = do
    print "createTask"
    dagligvarerId <- findOption "LIST_ID" return
    request (listUrl dagligvarerId)
        <&> Http.setRequestMethod "POST"
        <&> Http.setRequestBodyJSON payload
        >>= httpWithReauth
        >>= (withExceptT OutlookResponseError . Request.unwrap)

updateTask :: Task -> ExceptT Error IO Task
updateTask t = do
    print "updateTask"
    dagligvarerId <- findOption "LIST_ID" return
    request (taskUrl dagligvarerId (Task.id t))
        <&> Http.setRequestMethod "PUT"
        <&> Http.setRequestBodyJSON t
        >>= httpWithReauth
        >>= (withExceptT OutlookResponseError . Request.unwrap)

getTasks :: ExceptT Error IO [Task]
getTasks = do
    print "getTasks"
    dagligvarerId <- findOption "LIST_ID" return
    request (listUrl dagligvarerId)
        >>= httpWithReauth 
        >>= (withExceptT OutlookResponseError . Request.unwrap)

getLists :: ExceptT Error IO [List.List]
getLists = do
    print "getLists"
    request listListUrl
        >>= httpWithReauth 
        >>= (withExceptT OutlookResponseError . Request.unwrap)
