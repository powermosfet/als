module Outlook.Auth 
    ( Error
    , createToken
    , authenticate
    , refresh
    ) where

import Protolude

import Web.Browser (openBrowser)
import qualified Config
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Status as Status
import qualified Outlook.Request as OutlookRequest
import qualified Outlook.AuthToken

data Error
  = ConfigError Config.Error
  | HttpError Http.HttpException
  | OutlookRequestError OutlookRequest.Error
  deriving (Show)

authenticationUrl :: ExceptT Error IO ByteString
authenticationUrl = do
  clientId <- withExceptT ConfigError $ Config.findOption "CLIENT_ID" Config.bytestring
  redirectUrl <- withExceptT ConfigError $ Config.findOption "REDIRECT_URL" Config.bytestring
  return $ "https://login.microsoftonline.com/common/oauth2/v2.0/authorize?" <>
    BS.intercalate "&"
      [ "client_id=" <> clientId
      , "response_type=code"
      , "redirect_uri=" <> redirectUrl
      , "scope=offline_access%20Tasks.ReadWrite"
      , "response_mode=query"
      , "state=0"
      ]

tokenUrl :: [Char]
tokenUrl =
  "https://login.microsoftonline.com/common/oauth2/v2.0/token"

authenticate :: ExceptT Error IO ()
authenticate = do
  r <- authenticationUrl
  print r
  code <- liftIO BS.getLine
  tokenResponse <- createToken code
  putStrLn ("ACCESS_TOKEN='" <> Outlook.AuthToken.access_token tokenResponse <> "'")
  putStrLn ("REFRESH_TOKEN='" <> Outlook.AuthToken.refresh_token tokenResponse <> "'")

createToken :: ByteString -> ExceptT Error IO (Outlook.AuthToken.AuthToken)
createToken code = do
    clientId <- withExceptT ConfigError $ Config.findOption "CLIENT_ID" Config.bytestring
    redirectUrl <- withExceptT ConfigError $ Config.findOption "REDIRECT_URL" Config.bytestring
    tokenUrl
        & Http.parseRequest
        & withExceptT HttpError
        <&> Http.setRequestBodyURLEncoded
          [ ("client_id", clientId)
          , ("scope", "offline_access Tasks.ReadWrite")
          , ("grant_type", "authorization_code")
          , ("code", code)
          , ("redirect_uri", redirectUrl)
          ]
        >>= (withExceptT OutlookRequestError . OutlookRequest.send)

refresh :: ExceptT Error IO (Outlook.AuthToken.AuthToken)
refresh = do
    clientId <- withExceptT ConfigError $ Config.findOption "CLIENT_ID" Config.bytestring
    refreshToken <- withExceptT ConfigError $ Config.findOption "REFRESH_TOKEN" Config.bytestring
    redirectUrl <- withExceptT ConfigError $ Config.findOption "REDIRECT_URL" Config.bytestring
    tokenUrl
        & Http.parseRequest
        & withExceptT HttpError
        <&> Http.setRequestBodyURLEncoded
          [ ("client_id", clientId)
          , ("grant_type", "refresh_token")
          , ("scope", "offline_access Tasks.ReadWrite")
          , ("refresh_token", refreshToken)
          , ("redirect_uri", redirectUrl)
          ]
        >>= (withExceptT OutlookRequestError . OutlookRequest.send)
