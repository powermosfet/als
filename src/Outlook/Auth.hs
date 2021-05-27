module Outlook.Auth 
    ( Error
    , authenticate
    ) where

import Protolude

import Web.Browser (openBrowser)
import qualified Config
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Status as Status
import qualified Outlook.AuthToken

data Error
  = ConfigError Config.Error
  | HttpException Http.HttpException
  deriving (Show)

createToken :: [Char] -> [Char] -> [Char] -> ExceptT Error IO (Http.Response Outlook.AuthToken.AuthToken)
createToken clientId redirectUrl code = do
    req <- "https://login.microsoftonline.com/common/oauth2/v2.0/token"
        & Http.parseRequest
        <&> Http.setRequestBodyURLEncoded
          [ ("client_id", BS.pack clientId)
          , ("code", BS.pack code)
          , ("scope", "Tasks.ReadWrite")
          , ("redirect_uri", BS.pack redirectUrl)
          , ("grant_type", "authorization_code")
          ]
        & withExceptT HttpException
    liftIO (Http.httpJSON req)

authenticate :: ExceptT Error IO ()
authenticate = do
    clientId <- withExceptT ConfigError $ Config.findOption "CLIENT_ID" Config.string
    redirectUrl <- withExceptT ConfigError $ Config.findOption "REDIRECT_URL" Config.string
    let url =  "https://login.microsoftonline.com/common/oauth2/v2.0/authorize?client_id=" ++ clientId ++ "&scope=Tasks.ReadWrite&response_type=code&redirect_uri=" ++ redirectUrl
    print url
    liftIO $ openBrowser url
    liftIO $ putStr ("Enter code: " :: Text)
    code <- liftIO $ getLine
    tokenResult <- createToken clientId redirectUrl (T.unpack code)
    print $ Http.getResponseBody tokenResult
