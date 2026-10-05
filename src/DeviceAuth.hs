{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DeviceAuth where
import Prelude
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Time (getCurrentTime)
import Data.Maybe (listToMaybe)
import qualified GraphClient as Graph
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (responseTimeoutMicro)
import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import TokenStore

data AuthConfig = AuthConfig { authClientId :: T.Text, authTimeout :: Int, authTokenFile :: Maybe FilePath }
data AuthEndpoints = AuthEndpoints { deviceEndpoint :: String, authTokenEndpoint :: String }
production :: AuthEndpoints
production = AuthEndpoints "https://login.microsoftonline.com/common/oauth2/v2.0/devicecode"
                           "https://login.microsoftonline.com/common/oauth2/v2.0/token"

data DeviceCode = DeviceCode
  { deviceCode :: T.Text, userCode :: T.Text, verificationUri :: T.Text
  , expiresIn :: Int, pollInterval :: Int }
instance FromJSON DeviceCode where
  parseJSON = withObject "device authorization" $ \o -> do
    code <- o .: "device_code"
    user <- o .: "user_code"
    uri <- o .: "verification_uri"
    expiry <- o .: "expires_in"
    interval <- o .:? "interval" .!= 5
    if all (not . T.null . T.strip) [code,user,uri] && expiry > 0 &&
       expiry <= (maxBound `div` 1000000 :: Int) && interval > 0 && interval <= expiry
      then pure (DeviceCode code user uri expiry interval)
      else fail "invalid device authorization response"
newtype OAuthError = OAuthError T.Text
instance FromJSON OAuthError where
  parseJSON = withObject "OAuth error" (\o -> OAuthError <$> o .: "error")

parseAuthConfig :: [(String,String)] -> Either String AuthConfig
parseAuthConfig env = do
  client <- case lookup "CLIENT_ID" env of
    Just value | not (T.null (T.strip (T.pack value))) -> Right (T.pack value)
    _ -> Left "Missing or invalid CLIENT_ID"
  seconds <- case lookup "HTTP_TIMEOUT_SECONDS" env of
    Nothing -> Right 30
    Just value -> case readMaybe value of
      Just n | n > 0 && n <= (maxBound `div` 1000000 :: Int) -> Right n
      _ -> Left "Invalid HTTP_TIMEOUT_SECONDS"
  file <- case lookup "TOKEN_FILE" env of
    Just value | T.null (T.strip (T.pack value)) -> Left "Invalid TOKEN_FILE"
    value -> Right value
  pure (AuthConfig client seconds file)

loadAuthConfig :: IO (Either String AuthConfig)
loadAuthConfig = do
  pairs <- mapM (\k -> fmap ((,) k) <$> lookupEnv k) ["CLIENT_ID","TOKEN_FILE","HTTP_TIMEOUT_SECONDS"]
  pure (parseAuthConfig [pair | Just pair <- pairs])

-- Inject time, waiting and presentation so expiration and polling are testable
-- without waiting for a real browser or calling Microsoft's endpoints.
authenticateWith :: AuthConfig -> AuthEndpoints -> IO Double -> (Int -> IO ()) -> (T.Text -> IO ())
                 -> IO (Either String Tokens)
authenticateWith cfg endpoints now wait present = do
  request <- H.parseRequest (deviceEndpoint endpoints)
  response <- sendAuth cfg (H.setRequestBodyURLEncoded
    [("client_id",TE.encodeUtf8 (authClientId cfg)),("scope","offline_access Tasks.ReadWrite")] request)
  case response of
    Left failure -> pure (Left failure)
    Right r | statusCode (H.getResponseStatus r) == 429 || statusCode (H.getResponseStatus r) >= 500 ->
                pure (Left "Device authorization temporarily unavailable; run authentication again")
            | statusCode (H.getResponseStatus r) /= 200 -> pure (Left "Device authorization failed; check the app's public-client settings")
            | otherwise -> case eitherDecode (fromStrict (H.getResponseBody r)) of
        Left _ -> pure (Left "Device authorization response invalid")
        Right device -> do
          started <- now
          let deadline = started + fromIntegral (expiresIn device)
          present ("Open " <> verificationUri device <> " in a browser on any device.")
          present ("Enter code: " <> userCode device)
          present "Waiting for sign-in; press Ctrl+C to cancel."
          tokenRequest <- H.parseRequest (authTokenEndpoint endpoints)
          let pollRequest = H.setRequestBodyURLEncoded
                [("client_id",TE.encodeUtf8 (authClientId cfg)),
                 ("grant_type","urn:ietf:params:oauth:grant-type:device_code"),
                 ("device_code",TE.encodeUtf8 (deviceCode device))] tokenRequest
              poll interval = do
                current <- now
                if current >= deadline then pure (Left "Sign-in code expired; run authentication again")
                else if current + fromIntegral interval >= deadline
                  then wait (ceiling (deadline - current)) >> pure (Left "Sign-in code expired; run authentication again")
                  else do
                    wait interval
                    remaining <- (deadline -) <$> now
                    if remaining <= 0 then pure (Left "Sign-in code expired; run authentication again") else do
                      result <- timeout (floor (remaining * 1000000)) (sendAuth cfg pollRequest)
                      after <- now
                      if after >= deadline then pure (Left "Sign-in code expired; run authentication again") else
                        case result of
                          Nothing -> pure (Left "Sign-in code expired; run authentication again")
                          -- Transport failures back off too, bounded by the expiry.
                          Just (Left _) -> poll (min (expiresIn device) (interval * 2))
                          Just (Right r') ->
                            if statusCode (H.getResponseStatus r') == 200 then
                              pure $ case eitherDecode (fromStrict (H.getResponseBody r')) of
                                Right tokens | validTokens tokens -> Right tokens
                                _ -> Left "Authentication response invalid"
                            else if statusCode (H.getResponseStatus r') >= 500 || statusCode (H.getResponseStatus r') == 429
                              then do
                                wallClock <- getCurrentTime
                                let retry = maybe 0 (Graph.retryAfter wallClock)
                                      (listToMaybe (H.getResponseHeader "Retry-After" r'))
                                poll (min (expiresIn device) (max retry (interval * 2)))
                            else case eitherDecode (fromStrict (H.getResponseBody r')) of
                              Right (OAuthError "authorization_pending") -> poll interval
                              Right (OAuthError "slow_down") -> poll (min (expiresIn device) (interval + 5))
                              Right (OAuthError "authorization_declined") -> pure (Left "Sign-in declined; existing tokens were not changed")
                              Right (OAuthError "access_denied") -> pure (Left "Sign-in declined; existing tokens were not changed")
                              Right (OAuthError "expired_token") -> pure (Left "Sign-in code expired; run authentication again")
                              _ -> pure (Left "Device sign-in failed; check the app and account permissions")
          poll (pollInterval device)

sendAuth :: AuthConfig -> H.Request -> IO (Either String (H.Response B.ByteString))
sendAuth cfg request = do
  result <- try $ H.httpBS $ H.setRequestIgnoreStatus $
    H.setRequestResponseTimeout (responseTimeoutMicro (authTimeout cfg * 1000000)) request
  pure $ case result of
    Left (_ :: H.HttpException) -> Left "Authentication transport failure"
    Right response -> Right response

-- Persist only a complete successful token pair. Cancellation leaves the old
-- file intact; a save failure is reported and never treated as successful login.
authenticate :: IO (Either String ())
authenticate = do
  config <- loadAuthConfig
  case config of
    Left failure -> pure (Left failure)
    Right cfg -> do
      result <- authenticateWith cfg production
        ((/ 1000000000) . fromIntegral <$> getMonotonicTimeNSec) sleep (putStrLn . T.unpack)
      case result of
        Left failure -> pure (Left failure)
        Right tokens -> saveAuthentication cfg (putStrLn . T.unpack) tokens
 where sleep seconds | seconds <= 0 = pure ()
                     | otherwise = threadDelay 1000000 >> sleep (seconds - 1)

saveAuthentication :: AuthConfig -> (T.Text -> IO ()) -> Tokens -> IO (Either String ())
saveAuthentication cfg present tokens = case authTokenFile cfg of
  Just path -> saveTokens path tokens
  Nothing -> do
    present ("ACCESS_TOKEN=" <> access_token tokens)
    present ("REFRESH_TOKEN=" <> refresh_token tokens)
    pure (Right ())
