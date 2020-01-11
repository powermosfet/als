{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Outlook
    ( Auth(..)
    , Error(..)
    , createTask
    ) where

import Protolude

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Char8 as B
import Outlook.CreateTask (CreateTask)
import Outlook.MoveTask (MoveTask(..))
import Outlook.Task (Task)
import System.Environment (setEnv)
import qualified Config
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Status as Status
import qualified Outlook.CreateTask as CreateTask 
import qualified Outlook.MoveTask as MoveTask 
import qualified Outlook.ResponseOrError as OutlookApiError
import qualified Outlook.Task as Task 

data Auth = Auth
    { access_token :: [Char]
    , refresh_token :: [Char]
    } deriving (Show, Generic, FromJSON)

getAuth :: ExceptT Error IO Auth
getAuth = do
    envAccessToken <- findOption "ACCESS_TOKEN" return
    envRefreshToken <- findOption "REFRESH_TOKEN" return
    return $ Auth envAccessToken envRefreshToken

setAuth :: Auth -> IO ()
setAuth (Auth {..}) = do
    setEnv "ACCESS_TOKEN" access_token
    setEnv "REFRESH_TOKEN" refresh_token

withRefresh :: ExceptT Error IO (Http.Response (OutlookApiError.ResponseOrError a)) -> ExceptT Error IO (Http.Response a)
withRefresh doRequest = do
    response <- doRequest
    if Http.getResponseStatus response == Status.unauthorized401 then
        refreshAccessToken >> doRequest >>= catchApiError 
    else
        catchApiError response

withAuth :: Auth -> Http.Request -> Http.Request
withAuth (Auth {..}) req =
    req
        & Http.addRequestHeader "Authorization" ("Bearer " <> B.pack access_token)

refreshAccessToken :: ExceptT Error IO ()
refreshAccessToken = do
    clientId <- findOption "CLIENT_ID" Config.bytestring
    refreshToken <- findOption "REFRESH_TOKEN" Config.bytestring
    req <- request "https://login.microsoftonline.com/common/oauth2/v2.0/token"
        <&> Http.setRequestMethod "POST"
        <&> Http.setRequestBodyURLEncoded
            [ ("client_id", clientId)
            , ("grant_type", "refresh_token")
            , ("refresh_token", refreshToken)
            , ("scope", "Tasks.ReadWrite offline_access")
            ]
    response <- liftIO $ Http.httpJSON req
    liftIO $ setAuth (Http.getResponseBody response)

allTasksUrl :: [Char]
allTasksUrl = "https://graph.microsoft.com/beta/me/outlook/tasks"

taskUrl :: Task.Id -> [Char]
taskUrl taskId = "https://graph.microsoft.com/beta/me/outlook/tasks/" <> taskId

request :: [Char] -> ExceptT Error IO Http.Request
request url = url
    & Http.parseRequest
    & withExceptT HttpException

data Error
    = ConfigError Config.Error
    | HttpException Http.HttpException
    | ApiError OutlookApiError.OutlookApiError
    deriving (Show)

catchApiError :: Http.Response (OutlookApiError.ResponseOrError a) -> ExceptT Error IO (Http.Response a)
catchApiError response =
    case Http.getResponseBody response of
        OutlookApiError.SuccessResponse aValue -> return (fmap (const aValue) response)
        OutlookApiError.ErrorResponse error -> throwError (ApiError error)

findOption :: [Char] -> ([Char] -> ExceptT Config.Error IO a) -> ExceptT Error IO a
findOption name convert = 
    Config.findOption name convert
        &   withExceptT ConfigError

createTask :: CreateTask -> ExceptT Error IO Task
createTask payload = do
    dagligvarerId <- findOption "LIST_ID" return
    let moveToDagligvarer = MoveTask dagligvarerId
    createResponse <- withRefresh (createTask' payload) 
    moveResponse <- withRefresh (moveTask moveToDagligvarer (Http.getResponseBody createResponse))
    return $ Http.getResponseBody moveResponse

createTask' :: CreateTask -> ExceptT Error IO (Http.Response (OutlookApiError.ResponseOrError Task))
createTask' payload = do
    auth <- getAuth
    req <- request allTasksUrl
        <&> withAuth auth
        <&> Http.setRequestMethod "POST"
        <&> Http.setRequestBodyJSON payload
    liftIO (Http.httpJSON req)

moveTask :: MoveTask -> Task -> ExceptT Error IO (Http.Response (OutlookApiError.ResponseOrError Task))
moveTask payload task = do
    let taskId = Task.id task
    auth <- getAuth
    req <- request (taskUrl taskId)
        <&> withAuth auth
        <&> Http.setRequestMethod "PATCH"
        <&> Http.setRequestBodyJSON payload
    liftIO (Http.httpJSON req)
