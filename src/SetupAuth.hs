{-# LANGUAGE NoImplicitPrelude #-}
module SetupAuth (authenticate) where
import Prelude
import Control.Exception (try)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Network.HTTP.Client (responseTimeoutMicro)
import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderQuery)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import TokenStore

-- Manual public-client OAuth flow. No listener or browser process is needed.
authenticate :: IO (Either String ())
authenticate = do
  client <- lookupEnv "CLIENT_ID"
  redirect <- lookupEnv "REDIRECT_URL"
  file <- lookupEnv "TOKEN_FILE"
  seconds <- lookupEnv "HTTP_TIMEOUT_SECONDS"
  let valid = maybe False (not . T.null . T.strip . T.pack)
      limit = case seconds of
        Nothing -> Just 30
        Just s -> readMaybe s
  case (client,redirect,limit) of
    (Just c,Just r,Just t) | valid client && valid redirect && t > 0 && t <= (maxBound `div` 1000000 :: Int)
                          && maybe True (not . null) file -> do
      let url = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize" <>
            renderQuery True [("client_id",Just (B.pack c)),("response_type",Just "code"),
              ("redirect_uri",Just (B.pack r)),("scope",Just "offline_access Tasks.ReadWrite"),
              ("response_mode",Just "query")]
      B.putStrLn url
      putStrLn "Open this URL in your browser. After consent, paste only the code from the redirect URL:"
      code <- B.getLine
      req <- H.parseRequest "https://login.microsoftonline.com/common/oauth2/v2.0/token"
      result <- try $ H.httpBS $ H.setRequestIgnoreStatus $
        H.setRequestResponseTimeout (responseTimeoutMicro (t * 1000000)) $
        H.setRequestBodyURLEncoded [("client_id",B.pack c),("redirect_uri",B.pack r),
          ("grant_type","authorization_code"),("code",code),("scope","offline_access Tasks.ReadWrite")] req
      case (result :: Either H.HttpException (H.Response B.ByteString)) of
        Left _ -> pure (Left "Authentication transport failure")
        Right response | statusCode (H.getResponseStatus response) /= 200 -> pure (Left "Authentication failed")
                       | otherwise -> case eitherDecode (fromStrict (H.getResponseBody response)) of
            Right tokens | validTokens tokens -> case file of
              Just path -> saveTokens path tokens
              Nothing -> do
                -- This is the sole deliberate credential output, for manual setup.
                B.putStrLn (fromStrictText ("ACCESS_TOKEN=" <> access_token tokens))
                B.putStrLn (fromStrictText ("REFRESH_TOKEN=" <> refresh_token tokens))
                pure (Right ())
            _ -> pure (Left "Authentication response invalid")
    _ -> pure (Left "Invalid CLIENT_ID, REDIRECT_URL, TOKEN_FILE or HTTP_TIMEOUT_SECONDS")
 where fromStrictText = B.pack . T.unpack
