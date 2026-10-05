{-# LANGUAGE OverloadedStrings #-}
module DeviceAuthSpec (deviceAuthTests) where
import Prelude
import Control.Exception (bracket, try, AsyncException(..), throwIO)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import Data.List (isInfixOf)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory
import System.FilePath
import System.IO
import Test.HUnit
import DeviceAuth
import TokenStore
import Lib (failureExitCode)

challenge :: BL.ByteString
challenge = encode (object ["device_code" .= ("private-device-code" :: T.Text),"user_code" .= ("ABCD-EFGH" :: T.Text),
  "verification_uri" .= ("https://microsoft.com/devicelogin" :: T.Text),"expires_in" .= (30 :: Int),"interval" .= (1 :: Int)])
rotated :: BL.ByteString
rotated = "{\"access_token\":\"new-access\",\"refresh_token\":\"new-refresh\"}"
config :: AuthConfig
config = AuthConfig "test-client" 2 Nothing

withStub :: [(Status,BL.ByteString)] -> (AuthEndpoints -> IORef [(Request,BL.ByteString)] -> IO a) -> IO a
withStub replies action = do
  pending <- newIORef replies
  requests <- newIORef []
  let app request respond = do
        body <- strictRequestBody request
        modifyIORef' requests (++ [(request,body)])
        (status,payload) <- atomicModifyIORef' pending $ \rs -> case rs of
          r:rest -> (rest,r)
          [] -> ([],(status400,"unexpected poll"))
        respond (responseLBS status [("Content-Type","application/json")] payload)
  testWithApplication (pure app) $ \port -> do
    let base = "http://127.0.0.1:" ++ show port
    action (AuthEndpoints (base ++ "/devicecode") (base ++ "/token")) requests

simulate :: AuthEndpoints -> IO (Either String Tokens, [Int], [T.Text])
simulate endpoints = do
  clock <- newIORef (0 :: Double)
  waits <- newIORef []
  messages <- newIORef []
  result <- authenticateWith config endpoints (readIORef clock)
    (\seconds -> modifyIORef' waits (++ [seconds]) >> modifyIORef' clock (+ fromIntegral seconds))
    (\msg -> modifyIORef' messages (++ [msg]))
  (,,) result <$> readIORef waits <*> readIORef messages

deviceAuthTests :: Test
deviceAuthTests = TestLabel "device-code authentication" $ TestList
  [ TestCase $ do
      assertBool "no redirect needed" $ case parseAuthConfig [("CLIENT_ID","client")] of Right _ -> True; _ -> False
      forM_ [[],[("CLIENT_ID","")],[("CLIENT_ID","c"),("TOKEN_FILE"," ")],
              [("CLIENT_ID","c"),("HTTP_TIMEOUT_SECONDS","0")]] $ \env ->
        assertBool "invalid configuration" $ case parseAuthConfig env of Left _ -> True; _ -> False
  , TestCase $ withStub [(status200,challenge),(status400,"{\"error\":\"authorization_pending\"}"),
                        (status400,"{\"error\":\"slow_down\"}"),(status200,rotated)] $ \endpoints requests -> do
      (result,waits,messages) <- simulate endpoints
      assertEqual "token pair" (Right (Tokens "new-access" "new-refresh")) result
      assertEqual "pending + permanent slowdown" [1,1,6] waits
      assertBool "display short code" (any (T.isInfixOf "ABCD-EFGH") messages)
      assertBool "never display device/token secrets" (all (\m -> not (any (`T.isInfixOf` m)
        ["private-device-code","new-access","new-refresh"])) messages)
      reqs <- readIORef requests
      assertEqual "device endpoint" "/devicecode" (rawPathInfo (fst (head reqs)))
      assertBool "offline + task scopes" ("scope=offline_access%20Tasks.ReadWrite" `isInfixOf` show (snd (head reqs))
         || "scope=offline_access+Tasks.ReadWrite" `isInfixOf` show (snd (head reqs)))
      assertBool "device grant" ("grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code" `isInfixOf` show (snd (reqs !! 1)))
  , TestCase $ forM_ ["authorization_declined","access_denied","expired_token","bad_verification_code"] $ \code ->
      withStub [(status200,challenge),(status400,encode (object ["error" .= (code :: T.Text)]))] $ \endpoints requests -> do
        (result,_,_) <- simulate endpoints
        assertBool "failure stops polling" $ case result of Left _ -> True; _ -> False
        assertEqual "single poll" 2 . length =<< readIORef requests
  , TestCase $ do
      let short = "{\"device_code\":\"secret\",\"user_code\":\"code\",\"verification_uri\":\"https://example.com\",\"expires_in\":2,\"interval\":1}"
      withStub [(status200,short),(status400,"{\"error\":\"authorization_pending\"}")] $ \endpoints requests -> do
        (result,waits,_) <- simulate endpoints
        assertEqual "local expiry" (Left "Sign-in code expired; run authentication again") result
        assertEqual "wait full lifetime" [1,1] waits
        assertEqual "no poll after expiry" 2 . length =<< readIORef requests
  , TestCase $ forM_ ["{}","{\"device_code\":\"secret\",\"user_code\":\"c\",\"verification_uri\":\"u\",\"expires_in\":0}"] $ \body ->
      withStub [(status200,body)] $ \endpoints requests -> do
        (result,_,_) <- simulate endpoints
        assertEqual "bad challenge" (Left "Device authorization response invalid") result
        assertEqual "never poll bad challenge" 1 . length =<< readIORef requests
  , TestCase $ withStub [(status200,challenge),(status200,"{\"access_token\":\"only-one\"}")] $ \endpoints _ -> do
      (result,_,_) <- simulate endpoints
      assertEqual "require both tokens" (Left "Authentication response invalid") result
  , TestCase $ withStub [(status200,challenge),(status503,"{}"),(status200,rotated)] $ \endpoints _ -> do
      (result,waits,_) <- simulate endpoints
      assertEqual "transient poll recovery" (Right (Tokens "new-access" "new-refresh")) result
      assertEqual "backoff" [1,2] waits
  , TestCase $ do
      calls <- newIORef (0 :: Int)
      let app _ respond = do
            n <- atomicModifyIORef' calls (\i -> (i+1,i))
            respond $ case n of
              0 -> responseLBS status200 [] challenge
              1 -> responseLBS status429 [("Retry-After","7")] "{}"
              _ -> responseLBS status200 [] rotated
      testWithApplication (pure app) $ \port -> do
        let base = "http://127.0.0.1:" ++ show port
        (result,waits,_) <- simulate (AuthEndpoints (base ++ "/devicecode") (base ++ "/token"))
        assertEqual "throttled polling" (Right (Tokens "new-access" "new-refresh")) result
        assertEqual "respect Retry-After" [1,7] waits
  , TestCase $ withStub [(status200,challenge)] $ \endpoints requests -> do
      result <- try (authenticateWith config endpoints (pure 0)
        (const (throwIO UserInterrupt)) (const (pure ()))) :: IO (Either AsyncException (Either String Tokens))
      assertEqual "cancellation propagates" (Left UserInterrupt) result
      assertEqual "no poll on cancellation" 1 . length =<< readIORef requests
  , TestCase $ bracket temporary removePathForcibly $ \dir -> do
      let file = dir </> "tokens.json"
          cfg = config {authTokenFile=Just file}
          old = Tokens "old-access" "old-refresh"
      _ <- saveTokens file old
      withStub [(status200,challenge),(status400,"{\"error\":\"authorization_declined\"}")] $ \endpoints _ -> do
        (result,_,_) <- simulate endpoints
        forM_ result $ \tokens -> void (saveAuthentication cfg (const (assertFailure "secret output")) tokens)
        assertEqual "failed login preserves file" (Right old) =<< loadTokens (Just file)
      withStub [(status200,challenge),(status200,rotated)] $ \endpoints _ -> do
        (result,_,_) <- simulate endpoints
        case result of
          Right tokens -> assertEqual "saved login" (Right ()) =<< saveAuthentication cfg (const (assertFailure "secret output")) tokens
          Left e -> assertFailure e
        assertEqual "rotated pair saved" (Right (Tokens "new-access" "new-refresh")) =<< loadTokens (Just file)
      assertEqual "failed persistence" (Left "Token persistence failed") =<<
        saveAuthentication (cfg {authTokenFile=Just (dir </> "absent" </> "tokens")}) (const (pure ())) old
  , TestCase $ do
      forM_ ["authentication-required","authentication-failed","token-refresh-failed","Token file invalid"] $ \category ->
        assertEqual "requires interaction" 78 (failureExitCode category)
      forM_ ["authentication-cancelled","token-persistence-failed","permission-denied","list-missing","worker-failed"] $ \category ->
        assertEqual "ordinary failure" 1 (failureExitCode category)
  ]
 where temporary = do
         tmp <- getTemporaryDirectory
         (name,h) <- openTempFile tmp "als-device-auth"
         hClose h
         removeFile name
         createDirectory name
         pure name
