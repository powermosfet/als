{-# LANGUAGE OverloadedStrings #-}
module Integration (integrationTests) where
import Prelude
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Network.AMQP
import Network.AMQP.Types (FieldTable(..), FieldValue(..))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Environment
import System.Process
import System.Timeout
import Test.HUnit
import qualified GraphClient as G
import TokenStore
import Worker
import WorkerConfig

integrationTests :: IO Test
integrationTests = do
  port <- lookupEnv "ALS_INTEGRATION_PORT"
  pure $ case port of
    Nothing -> TestLabel "integration (run nix flake check)" (TestList [])
    Just s -> TestLabel "disposable RabbitMQ + HTTP stub" (TestCase (integration (read s)))

within :: String -> IO a -> IO a
within label action = do
  result <- timeout 20000000 action
  case result of
    Nothing -> assertFailure (label ++ " timed out") >> error "timeout"
    Just value -> pure value

waitFor :: String -> IO Bool -> IO ()
waitFor label check = within label loop
 where loop = do
         done <- check
         unless done (threadDelay 50000 >> loop)

integration :: Int -> IO ()
integration port = do
  let config = either error id $ parseConfig
        [("LIST_ID","list"),("CLIENT_ID","client"),("RABBITMQ_HOST","127.0.0.1"),
         ("RABBITMQ_PORT",show port),("RETRY_DELAY_SECONDS","1"),("HTTP_TIMEOUT_SECONDS","10")]
      connect = openConnection' "127.0.0.1" (fromIntegral port) "/" "guest" "guest"
      withChannel action = bracket connect closeConnection (\conn -> openChannel conn >>= action)
      q = queue config
      dead = "als-test-dead"
      qopts = newQueue {queueName=q, queueHeaders=FieldTable (M.fromList
        [("x-dead-letter-exchange",FVString ""),("x-dead-letter-routing-key",FVString "als-test-dead")])}
      publish chan body = publishMsg chan "" q newMsg {msgBody=body,msgDeliveryMode=Just Persistent}
      count chan name = do
        (_,n,_) <- declareQueue chan newQueue {queueName=name,queuePassive=True}
        pure n
      empty chan = do
        m <- getMsg chan Ack q
        case m of
          Nothing -> pure ()
          Just (_,env) -> ackEnv env >> assertFailure "delivery was not acknowledged"
  withChannel $ \chan -> do
    void $ declareQueue chan newQueue {queueName=dead}
    void $ declareQueue chan qopts
  calls <- newIORef ([] :: [T.Text])
  block <- newIORef False
  entered <- newEmptyMVar
  release <- newEmptyMVar
  let app req respond = do
        body <- strictRequestBody req
        let title = case decode body of
              Just (Object o) -> case fromJSON (Object o) :: Result TestTask of
                Success (TestTask t) -> t
                _ -> "invalid"
              _ -> "invalid"
        modifyIORef' calls (++ [title])
        shouldBlock <- atomicModifyIORef' block (\b -> (False,b))
        when shouldBlock (putMVar entered () >> takeMVar release)
        respond $ responseLBS status201 [("Content-Type","application/json")]
          (encode (object ["id" .= ("task" :: T.Text),"title" .= title,"status" .= ("notStarted" :: T.Text)]))
  testWithApplication (pure app) $ \httpPort -> do
    let url = "http://127.0.0.1:" ++ show httpPort
    client <- G.newClient config (G.Endpoints url (url ++ "/token")) (Tokens "test" "test")
    withAsync (runWorker config client) $ \worker -> do
      withChannel $ \chan -> do
        forM_ ["milk","milk"] $ \title -> publish chan (encode (object ["description" .= (title :: T.Text)]))
        publish chan "{\"description\":null}"
        waitFor "valid creates" ((==2) . length <$> readIORef calls)
        waitFor "rejected dead-letter" ((==1) <$> count chan dead)
        -- Stopping the consumer makes any missing acknowledgement visible again.
        cancel worker
        empty chan
        Just (bad,env) <- getMsg chan Ack dead
        assertEqual "dead-letter body" "{\"description\":null}" (msgBody bad)
        ackEnv env
    assertEqual "repeat titles create separately" ["milk","milk"] =<< readIORef calls
    -- Interrupt a request after the stub receives it. The unacked delivery
    -- must return, and prefetch one must leave the second message ready.
    writeIORef block True
    withAsync (runWorker config client) $ \worker -> withChannel $ \chan -> do
      publish chan "{\"description\":\"interrupted\"}"
      within "HTTP entered" (takeMVar entered)
      publish chan "{\"description\":\"next\"}"
      waitFor "prefetch one" ((==1) <$> count chan q)
      cancel worker
      waitFor "returned unacked delivery" ((==2) <$> count chan q)
      Just (msg,env) <- getMsg chan Ack q
      assertBool "redelivery marked" (envRedelivered env)
      assertEqual "interrupted message retained" "{\"description\":\"interrupted\"}" (msgBody msg)
      rejectEnv env True
      putMVar release ()
    withAsync (runWorker config client) $ \worker -> do
      waitFor "redelivered task created" ((==5) . length <$> readIORef calls)
      -- RabbitMQ processes acknowledgements before subsequent channel commands;
      -- observe queue idle before cancellation and inspect it afterward.
      threadDelay 100000
      cancel worker
      withChannel empty
    -- Keep a worker alive while the broker application is stopped and restarted.
    withAsync (runWorker config client) $ \worker -> do
      withChannel $ \chan -> waitFor "consumer ready" $ do
        (_,_,consumers) <- declareQueue chan newQueue {queueName=q,queuePassive=True}
        pure (consumers==1)
      within "stop broker" (callProcess "rabbitmqctl" ["stop_app"])
      threadDelay 1200000
      within "restart broker" (callProcess "rabbitmqctl" ["start_app"])
      withChannel $ \chan -> do
        publish chan "{\"description\":\"after restart\"}"
        waitFor "worker reconnected" ((==6) . length <$> readIORef calls)
        threadDelay 100000
        cancel worker
        empty chan
    assertEqual "stub receives redelivery and reconnection" ["milk","milk","interrupted","interrupted","next","after restart"] =<< readIORef calls

newtype TestTask = TestTask T.Text
instance FromJSON TestTask where
  parseJSON = withObject "task" (\o -> TestTask <$> o .: "title")
