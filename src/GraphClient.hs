{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GraphClient where
import Prelude
import Control.Exception (try)
import Data.Aeson (eitherDecode)
import Data.IORef
import Data.Maybe (listToMaybe)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Client (responseTimeoutMicro)
import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Delivery (Failure(..))
import qualified Outlook.CreateTask as C
import qualified Outlook.Task as Task
import TokenStore
import WorkerConfig
import Text.Read (readMaybe)

data Endpoints = Endpoints { graphBase :: String, tokenEndpoint :: String }
production :: Endpoints
production = Endpoints "https://graph.microsoft.com/v1.0" "https://login.microsoftonline.com/common/oauth2/v2.0/token"
data Client = Client WorkerConfig Endpoints (IORef Tokens) (Tokens -> IO (Either String ()))
newClient :: WorkerConfig -> Endpoints -> Tokens -> IO Client
newClient cfg endpoints tokens = newClientWithPersistence cfg endpoints tokens persist
 where persist t = maybe (pure (Right ())) (\file -> saveTokens file t) (tokenFile cfg)
newClientWithPersistence :: WorkerConfig -> Endpoints -> Tokens -> (Tokens -> IO (Either String ())) -> IO Client
newClientWithPersistence cfg endpoints tokens persist = Client cfg endpoints <$> newIORef tokens <*> pure persist

send :: WorkerConfig -> H.Request -> IO (Either Failure (H.Response B.ByteString))
send cfg req = do
  result <- try (H.httpBS (H.setRequestIgnoreStatus (H.setRequestResponseTimeout
                      (responseTimeoutMicro (timeoutSeconds cfg * 1000000)) req)))
  case result of
    Left (_ :: H.HttpException) -> pure (Left (Retry "http-transport" (retrySeconds cfg)))
    Right response -> do
      now <- getCurrentTime
      let status = statusCode (H.getResponseStatus response)
          wait = maybe (retrySeconds cfg) (max (retrySeconds cfg) . retryAfter now)
                   (listToMaybe (H.getResponseHeader "Retry-After" response))
      pure $ if status >= 200 && status < 300 then Right response
        else if status == 429 || status == 408 || status >= 500 then Left (Retry "http-transient" wait)
        else if status == 400 || status == 422 then Left InvalidItem
        else Left (Fatal (case status of
          401 -> "authentication-failed"
          403 -> "permission-denied"
          404 -> "list-missing"
          _ -> "http-definitive"))

retryAfter :: UTCTime -> B.ByteString -> Int
retryAfter now value = case readMaybe (B.unpack value) :: Maybe Integer of
  Just seconds -> fromInteger (max 0 (min 2147483647 seconds))
  Nothing -> case parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (B.unpack value) of
    Just date -> fromInteger (max 0 (min 2147483647 (ceiling (diffUTCTime date now))))
    Nothing -> 0

createTask :: Client -> T.Text -> IO (Either Failure ())
createTask client@(Client cfg endpoints tokens _) title = do
  req <- H.parseRequest (graphBase endpoints ++ "/me/todo/lists/" ++
                         B.unpack (urlEncode False (TE.encodeUtf8 (T.pack (listId cfg)))) ++ "/tasks")
  let post = H.setRequestMethod "POST" (H.setRequestBodyJSON (C.CreateTask title) req)
      attempt = do
        t <- readIORef tokens
        send cfg (H.setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 (access_token t)] post)
  first <- attempt
  result <- case first of
    Left (Fatal "authentication-failed") -> do
      refreshed <- refreshClient client
      case refreshed of
        Left failure -> pure (Left failure)
        Right () -> attempt
    _ -> pure first
  pure $ case result of
    Left failure -> Left failure
    Right response -> case eitherDecode (fromStrict (H.getResponseBody response)) :: Either String Task.Task of
      Left _ -> Left (Fatal "graph-response-invalid")
      Right _ -> Right ()

refreshClient :: Client -> IO (Either Failure ())
refreshClient (Client cfg endpoints ref persist) = do
  old <- readIORef ref
  req <- H.parseRequest (tokenEndpoint endpoints)
  result <- send cfg (H.setRequestBodyURLEncoded
    [("client_id",B.pack (clientId cfg)),("grant_type","refresh_token"),
     ("scope","offline_access Tasks.ReadWrite"),("refresh_token",TE.encodeUtf8 (refresh_token old))] req)
  case result of
    Left (Retry category seconds) -> pure (Left (Retry category seconds))
    Left _ -> pure (Left (Fatal "token-refresh-failed"))
    Right response -> case eitherDecode (fromStrict (H.getResponseBody response)) of
      Left _ -> pure (Left (Fatal "token-response-invalid"))
      Right new | not (validTokens new) -> pure (Left (Fatal "token-response-invalid"))
                | otherwise -> do
          saved <- persist new
          case saved of
            Left _ -> pure (Left (Fatal "token-persistence-failed"))
            Right () -> writeIORef ref new >> pure (Right ())
