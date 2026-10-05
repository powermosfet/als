{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Worker where
import Prelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, void)
import Network.AMQP
import System.Timeout (timeout)
import qualified Delivery as D
import qualified GraphClient as G
import WorkerConfig

data WorkerFailure = WorkerFailure String deriving (Show)
instance Exception WorkerFailure

logEvent :: String -> IO ()
logEvent = putStrLn

sleepSeconds :: Int -> IO ()
sleepSeconds seconds
  | seconds <= 0 = pure ()
  | otherwise = threadDelay 1000000 >> sleepSeconds (seconds - 1)

-- Catch synchronous exceptions only; async cancellation must reach brackets.
trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust (\e -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just e)

runWorker :: WorkerConfig -> G.Client -> IO ()
runWorker cfg client = forever $ do
  logEvent "broker-connecting"
  result <- trySync (session cfg client)
  case result of
    Left e -> case fromException e of
      Just (WorkerFailure category) -> throwIO (WorkerFailure category)
      Nothing -> logEvent "broker-disconnected"
    Right () -> logEvent "broker-disconnected"
  logEvent "broker-retry"
  sleepSeconds (retrySeconds cfg)

session :: WorkerConfig -> G.Client -> IO ()
session cfg client = bracket connect cleanup $ \conn -> do
  closed <- newEmptyTMVarIO
  let disconnected = void (atomically (tryPutTMVar closed ()))
  addConnectionClosedHandler conn True disconnected
  chan <- openChannel conn
  addChannelExceptionHandler chan (const disconnected)
  qos chan 0 1 False
  deliveries <- newTQueueIO
  void $ consumeMsgs' chan (queue cfg) Ack
    (atomically . writeTQueue deliveries)
    (const disconnected) (queueHeaders newQueue)
  logEvent "broker-consuming"
  void $ race (atomically (takeTMVar closed)) $ forever $ do
    (msg,env) <- atomically (readTQueue deliveries)
    result <- D.processDelivery D.DeliveryOps
      { D.create = G.createTask client
      , D.acknowledge = ackEnv env
      , D.reject = rejectEnv env False
      , D.delay = sleepSeconds
      , D.event = logEvent
      } (msgBody msg)
    case result of
      Left category -> throwIO (WorkerFailure category)
      Right () -> pure ()
 where
  connect = openConnection' (brokerHost cfg) (fromIntegral (brokerPort cfg))
              (brokerVhost cfg) (brokerUser cfg) (brokerPassword cfg)
  cleanup conn = do
    -- Best effort when the broker has disappeared, bounded on shutdown.
    void (timeout 5000000 (trySync (closeConnection conn)))
    logEvent "broker-closed"
