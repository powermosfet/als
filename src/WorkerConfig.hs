{-# LANGUAGE NoImplicitPrelude #-}
module WorkerConfig where
import Prelude
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data WorkerConfig = WorkerConfig
  { brokerHost :: String, brokerPort :: Int, brokerVhost :: T.Text
  , brokerUser :: T.Text, brokerPassword :: T.Text, queue :: T.Text
  , retrySeconds :: Int, timeoutSeconds :: Int, listId :: String
  , clientId :: String, tokenFile :: Maybe FilePath
  } deriving (Eq)

-- Errors contain variable names only, never their values.
parseConfig :: [(String,String)] -> Either String WorkerConfig
parseConfig env = do
  host <- setting "RABBITMQ_HOST" "localhost"
  port <- number "RABBITMQ_PORT" 5672 65535
  vhost <- setting "RABBITMQ_VHOST" "/"
  user <- setting "RABBITMQ_USERNAME" "guest"
  password <- setting "RABBITMQ_PASSWORD" "guest"
  q <- setting "RABBITMQ_QUEUE" "shopping-list-items"
  retry <- number "RETRY_DELAY_SECONDS" 5 maxBound
  timeout <- number "HTTP_TIMEOUT_SECONDS" 30 (maxBound `div` 1000000)
  list <- required "LIST_ID"
  client <- required "CLIENT_ID"
  file <- traverse (nonblank "TOKEN_FILE") (lookup "TOKEN_FILE" env)
  pure (WorkerConfig host port (T.pack vhost) (T.pack user) (T.pack password)
        (T.pack q) retry timeout list client file)
 where
  nonblank key s | T.null (T.strip (T.pack s)) = Left ("Invalid " ++ key)
                 | otherwise = Right s
  setting key def = nonblank key (maybe def id (lookup key env))
  required key = maybe (Left ("Missing " ++ key)) (nonblank key) (lookup key env)
  number key def limit = case lookup key env of
    Nothing -> Right def
    Just s -> case readMaybe s of
      Just n | n > 0 && n <= limit -> Right n
      _ -> Left ("Invalid " ++ key)

loadConfig :: IO (Either String WorkerConfig)
loadConfig = do
  pairs <- mapM (\k -> fmap ((,) k) <$> lookupEnv k) keys
  pure (parseConfig [pair | Just pair <- pairs])
 where keys = ["RABBITMQ_HOST","RABBITMQ_PORT","RABBITMQ_VHOST","RABBITMQ_USERNAME",
               "RABBITMQ_PASSWORD","RABBITMQ_QUEUE","RETRY_DELAY_SECONDS",
               "HTTP_TIMEOUT_SECONDS","LIST_ID","CLIENT_ID","TOKEN_FILE"]
