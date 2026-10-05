{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Prelude hiding (log)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import Integration (integrationTests)
import DeviceAuthSpec (deviceAuthTests)
import Test.HUnit
import Delivery
import qualified GraphClient as G
import qualified Outlook.Task as Task
import qualified Outlook.Request as GraphResponse
import TokenStore
import WorkerConfig

baseEnv :: [(String,String)]
baseEnv = [("LIST_ID","list"),("CLIENT_ID","client")]
cfg :: WorkerConfig
cfg = either error id (parseConfig baseEnv)
tokens :: Tokens
tokens = Tokens "initial-access" "initial-refresh"

main :: IO ()
main = do
  integration <- integrationTests
  counts <- runTestTT (TestList [parsing,configuration,deliveryTests,tokenTests,httpTests,deviceAuthTests,integration])
  when (errors counts + failures counts /= 0) exitFailure

parsing :: Test
parsing = TestLabel "message parsing" $ TestList $
  [ TestCase $ assertEqual "exact mapping with extras" (Right (Item "  Milk 🥛  "))
      (parseItem (encode (object ["description" .= ("  Milk 🥛  " :: T.Text), "extra" .= True])))
  ] ++ [TestCase $ assertBool (show body) (isLeft (parseItem body)) |
      body <- ["not json","[]","null","{}","{\"description\":2}",
               "{\"description\":null}","{\"description\":\"\"}",
               "{\"description\":\" \\t\\n\"}","{\"description\":\"\\u2003\"}"]]
 where isLeft (Left _) = True
       isLeft _ = False

configuration :: Test
configuration = TestLabel "configuration" $ TestList
  [ TestCase $ do
      assertEqual "host" "localhost" (brokerHost cfg)
      assertEqual "port" 5672 (brokerPort cfg)
      assertEqual "vhost" "/" (brokerVhost cfg)
      assertEqual "user" "guest" (brokerUser cfg)
      assertEqual "password" "guest" (brokerPassword cfg)
      assertEqual "queue" "shopping-list-items" (queue cfg)
      assertEqual "retry" 5 (retrySeconds cfg)
      assertEqual "timeout" 30 (timeoutSeconds cfg)
  , TestCase $ assertBool "required" (bad [])
  , TestCase $ forM_ ["RABBITMQ_PORT","RETRY_DELAY_SECONDS","HTTP_TIMEOUT_SECONDS"] $ \k ->
      forM_ ["0","-1","word",""] $ \v -> assertBool (k ++ v) (bad ((k,v):baseEnv))
  , TestCase $ assertBool "port range" (bad (("RABBITMQ_PORT","65536"):baseEnv))
  , TestCase $ forM_ ["LIST_ID","CLIENT_ID","RABBITMQ_HOST","RABBITMQ_VHOST",
                      "RABBITMQ_QUEUE","RABBITMQ_USERNAME","RABBITMQ_PASSWORD","TOKEN_FILE"] $ \k ->
      assertBool k (bad ((k," "):baseEnv))
  , TestCase $ assertEqual "overrides" (Right "custom")
      (brokerHost <$> parseConfig (("RABBITMQ_HOST","custom"):baseEnv))
  ]
 where bad env = case parseConfig env of Left _ -> True; _ -> False

-- Inject creation, ack/reject, sleep and logging. Events reveal ordering.
runDelivery :: [Either Failure ()] -> BL.ByteString -> IO (Either String (), [String], [T.Text])
runDelivery results body = do
  remaining <- newIORef results
  events <- newIORef []
  titles <- newIORef []
  let record x = modifyIORef' events (++ [x])
      ops = DeliveryOps
        (\title -> do
           modifyIORef' titles (++ [title])
           record "create"
           atomicModifyIORef' remaining (\rs -> case rs of
             x:xs -> (xs,x)
             [] -> error "unexpected creation"))
        (record "ack") (record "reject") (\s -> record ("delay " ++ show s)) record
  result <- processDelivery ops body
  (,,) result <$> readIORef events <*> readIORef titles

deliveryTests :: Test
deliveryTests = TestLabel "delivery decisions" $ TestList
  [ TestCase $ do
      (result,ev,ts) <- runDelivery [Right ()] (encode (object ["description" .= ("  Milk 🥛  " :: T.Text)]))
      assertEqual "success" (Right ()) result
      assertEqual "ack after create" ["create","ack","task-created"] ev
      assertEqual "title unchanged" ["  Milk 🥛  "] ts
  , TestCase $ do
      (_,ev,_) <- runDelivery [] "{}"
      assertEqual "invalid reject" ["message-invalid","reject"] ev
  , TestCase $ do
      (_,ev,_) <- runDelivery [Left InvalidItem] "{\"description\":\"milk\"}"
      assertEqual "item reject" ["create","item-rejected","reject"] ev
  , TestCase $ do
      (_,ev,_) <- runDelivery [Left (Retry "network" 7), Right ()] "{\"description\":\"milk\"}"
      assertEqual "retry retains delivery" ["create","network","delay 7","create","ack","task-created"] ev
  , TestCase $ do
      (result,ev,_) <- runDelivery [Left (Fatal "authentication-failed")] "{\"description\":\"milk\"}"
      assertEqual "fatal" (Left "authentication-failed") result
      assertEqual "unacked" ["create","authentication-failed"] ev
  , TestCase $ do
      entered <- newEmptyMVar
      acked <- newIORef False
      let ops = DeliveryOps (\_ -> putMVar entered () >> threadDelay 10000000 >> pure (Right ()))
                    (writeIORef acked True) (assertFailure "unexpected reject") (const (pure ())) (const (pure ()))
      withAsync (processDelivery ops "{\"description\":\"milk\"}") $ \task ->
        takeMVar entered >> cancel task
      assertEqual "shutdown doesn't ack" False =<< readIORef acked
  , TestCase $ do
      -- Equal titles in separate deliveries always create twice.
      (_,a,_) <- runDelivery [Right ()] "{\"description\":\"milk\"}"
      (_,b,_) <- runDelivery [Right ()] "{\"description\":\"milk\"}"
      assertEqual "duplicates accepted" 2 (length (filter (=="create") (a++b)))
  ]

withTemp :: (FilePath -> IO a) -> IO a
withTemp action = bracket acquire removePathForcibly action
 where acquire = do
         root <- getTemporaryDirectory
         (path,h) <- openTempFile root "als-tests"
         hClose h
         removeFile path
         createDirectory path
         pure path

tokenTests :: Test
tokenTests = TestLabel "token storage" $ TestCase $ withTemp $ \dir -> do
  let file = dir </> "tokens.json"
  bracket ((,) <$> lookupEnv "ACCESS_TOKEN" <*> lookupEnv "REFRESH_TOKEN")
    (\(a,r) -> restore "ACCESS_TOKEN" a >> restore "REFRESH_TOKEN" r) $ \_ -> do
      setEnv "ACCESS_TOKEN" "env-access"
      setEnv "REFRESH_TOKEN" "env-refresh"
      assertEqual "missing bootstraps" (Right (Tokens "env-access" "env-refresh")) =<< loadTokens (Just file)
      assertEqual "write" (Right ()) =<< saveTokens file tokens
      assertEqual "file preferred" (Right tokens) =<< loadTokens (Just file)
      mode <- fileMode <$> getFileStatus file
      assertEqual "owner only" 0o600 (mode `intersectFileModes` 0o777)
      BL.writeFile file "{}"
      assertEqual "corrupt fails" (Left "Token file invalid") =<< loadTokens (Just file)
      assertEqual "unreadable existing directory" (Left "Token file unreadable") =<< loadTokens (Just dir)
      assertEqual "missing parent" (Left "Token persistence failed") =<< saveTokens (dir </> "missing" </> "tokens") tokens
      setFileMode file 0o000
      assertEqual "unreadable file" (Left "Token file unreadable") =<< loadTokens (Just file)
      setFileMode file 0o600
      createSymbolicLink (dir </> "absent") (dir </> "broken-link")
      assertEqual "broken existing symlink" (Left "Token file unreadable") =<< loadTokens (Just (dir </> "broken-link"))
      unsetEnv "ACCESS_TOKEN"
      unsetEnv "REFRESH_TOKEN"
      assertEqual "missing credentials" (Left "Missing or invalid ACCESS_TOKEN / REFRESH_TOKEN") =<< loadTokens Nothing
 where restore key = maybe (unsetEnv key) (setEnv key)

type Reply = (Status, ResponseHeaders, BL.ByteString)
withStub :: [Reply] -> (String -> IORef [(Request,BL.ByteString)] -> IO a) -> IO a
withStub replies action = do
  remaining <- newIORef replies
  requests <- newIORef []
  let app req respond = do
        body <- strictRequestBody req
        modifyIORef' requests (++ [(req,body)])
        (status,headers,payload) <- atomicModifyIORef' remaining $ \rs -> case rs of
          x:xs -> (xs,x)
          [] -> ([],(status500,[],"unexpected request"))
        respond (responseLBS status headers payload)
  testWithApplication (pure app) $ \port -> action ("http://127.0.0.1:" ++ show port) requests

httpTests :: Test
httpTests = TestLabel "Graph HTTP and refresh" $ TestList
  [ TestCase $ do
      fixture <- BL.readFile "test/fixtures/task.json"
      assertBool "task response decoded directly" $ case eitherDecode fixture :: Either String Task.Task of
        Right task -> Task.title task == "  Milk 🥛  "
        _ -> False
      withStub [(status201,[],fixture)] $ \url requests -> do
        client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
        assertEqual "created" (Right ()) =<< G.createTask client "  Milk 🥛  "
        [(req,body)] <- readIORef requests
        assertEqual "POST" "POST" (requestMethod req)
        assertEqual "task path" "/me/todo/lists/list/tasks" (rawPathInfo req)
        assertEqual "body" (Just (object ["title" .= ("  Milk 🥛  " :: T.Text)])) (decode body)
  , TestCase $ withTemp $ \dir -> do
      task <- BL.readFile "test/fixtures/task.json"
      rotated <- BL.readFile "test/fixtures/tokens.json"
      withStub [(status401,[],"{}"),(status200,[],rotated),(status201,[],task),
                (status401,[],"{}"),(status200,[],rotated),(status201,[],task)] $ \url requests -> do
        let file = dir </> "tokens"
        client <- G.newClient (cfg {tokenFile=Just file}) (G.Endpoints url (url ++ "/token")) tokens
        assertEqual "refresh on 401" (Right ()) =<< G.createTask client "milk"
        assertEqual "both persisted" (Right (Tokens "rotated-access" "rotated-refresh")) =<< loadTokens (Just file)
        assertEqual "second refresh" (Right ()) =<< G.createTask client "milk"
        reqs <- readIORef requests
        assertEqual "one refresh each" 6 (length reqs)
        assertEqual "access token rotated" (Just "Bearer rotated-access") (lookup "Authorization" (requestHeaders (fst (reqs !! 2))))
        assertBool "refresh token rotated in memory" ("refresh_token=rotated-refresh" `isInfixOf` show (snd (reqs !! 4)))
  , TestCase $ do
      rotated <- BL.readFile "test/fixtures/tokens.json"
      withStub [(status401,[],"{}"),(status200,[],rotated)] $ \url requests -> do
        client <- G.newClientWithPersistence cfg (G.Endpoints url (url ++ "/token")) tokens
                    (const (pure (Left "disk failed")))
        assertEqual "persistence stops request" (Left (Fatal "token-persistence-failed")) =<< G.createTask client "milk"
        assertEqual "no task retry" 2 . length =<< readIORef requests
  , TestCase $ withTemp $ \dir -> do
      rotated <- BL.readFile "test/fixtures/tokens.json"
      withStub [(status401,[],"{}"),(status200,[],rotated)] $ \url requests -> do
        client <- G.newClient (cfg {tokenFile=Just (dir </> "missing" </> "tokens")})
                    (G.Endpoints url (url ++ "/token")) tokens
        assertEqual "real token file failure" (Left (Fatal "token-persistence-failed")) =<< G.createTask client "milk"
        assertEqual "no request after file failure" 2 . length =<< readIORef requests
  , TestCase $ withStub [(status401,[],"{}"),(status400,[],"{}")] $ \url _ -> do
      client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
      assertEqual "invalid refresh credentials retain delivery" (Left (Fatal "token-refresh-failed")) =<< G.createTask client "milk"
  , TestCase $ withStub [(status201,[],"{\"value\":[]}")] $ \url _ -> do
      client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
      assertEqual "creation doesn't accept collection wrapper" (Left (Fatal "graph-response-invalid")) =<< G.createTask client "milk"
  , TestCase $ assertBool "list operations retain collection wrapper" $ case
      eitherDecode "{\"value\":[]}" :: Either String (GraphResponse.Response [Task.Task]) of
        Right (GraphResponse.SuccessResponse _) -> True
        _ -> False
  , TestCase $ do
      rotated <- BL.readFile "test/fixtures/tokens.json"
      withStub [(status401,[],"{}"),(status200,[],rotated),(status401,[],"{}")] $ \url requests -> do
        client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
        assertEqual "refresh once" (Left (Fatal "authentication-failed")) =<< G.createTask client "milk"
        assertEqual "bounded" 3 . length =<< readIORef requests
  , TestCase $ forM_ [(status400,InvalidItem),(status422,InvalidItem),
                      (status403,Fatal "permission-denied"),(status404,Fatal "list-missing"),
                      (status500,Retry "http-transient" 5),(status408,Retry "http-transient" 5)] $ \(status,expected) ->
      withStub [(status,[],"{}")] $ \url _ -> do
        client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
        assertEqual (show status) (Left expected) =<< G.createTask client "milk"
  , TestCase $ withStub [(status429,[("Retry-After","7")],"{}")] $ \url _ -> do
      client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
      assertEqual "retry-after" (Left (Retry "http-transient" 7)) =<< G.createTask client "milk"
  , TestCase $ do
      let now = UTCTime (fromGregorian 2026 1 1) 0
      assertEqual "HTTP date retry-after" 10 (G.retryAfter now "Thu, 01 Jan 2026 00:00:10 GMT")
      assertEqual "invalid retry-after" 0 (G.retryAfter now "garbage")
  , TestCase $ do
      -- Allocate then close a listener: its port reliably refuses connections.
      port <- newIORef ""
      withStub [] $ \url _ -> writeIORef port url
      url <- readIORef port
      client <- G.newClient cfg (G.Endpoints url (url ++ "/token")) tokens
      assertEqual "transport caught" (Left (Retry "http-transport" 5)) =<< G.createTask client "milk"
  ]
