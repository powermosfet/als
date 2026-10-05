{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (als, failureExitCode) where
import Prelude
import Control.Concurrent (myThreadId)
import Control.Exception
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Posix.Signals
import qualified GraphClient as G
import qualified SetupAuth
import qualified DeviceAuth
import TokenStore
import WorkerConfig
import Worker

als :: IO ()
als = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  mainThread <- myThreadId
  let stop = Catch (throwTo mainThread UserInterrupt)
  _ <- installHandler sigTERM stop Nothing
  _ <- installHandler sigINT stop Nothing
  result <- trySync $ catch (case args of
    ["--auth"] -> DeviceAuth.authenticate >>= either failStartup (const (logEvent "authentication-complete"))
    ["--auth-code"] -> SetupAuth.authenticate >>= either failStartup (const (logEvent "authentication-complete"))
    [] -> do
      cfg <- loadConfig >>= either failStartup pure
      loaded <- loadTokens (tokenFile cfg)
      tokens <- case loaded of
        Left "Missing or invalid ACCESS_TOKEN / REFRESH_TOKEN" -> failStartup "authentication-required"
        Left message -> failStartup message
        Right value -> pure value
      client <- G.newClient cfg G.production tokens
      logEvent "worker-starting"
      runWorker cfg client
    _ -> failStartup "Usage: als [--auth | --auth-code]")
    (\(e :: AsyncException) -> case e of
      UserInterrupt -> if null args then logEvent "worker-stopped"
                       else failStartup "authentication-cancelled"
      _ -> throwIO e)
  case result of
    Left e -> do
      let category = case fromException e of
            Just (WorkerFailure value) -> value
            Nothing -> "worker-failed"
      logEvent category
      if failureExitCode category == 78 then
        logEvent "Sign-in required: run als --auth (on the NixOS service, sudo als-auth)."
        else pure ()
      exitWith (ExitFailure (failureExitCode category))
    Right () -> pure ()
 where failStartup message = throwIO (WorkerFailure message)

-- Reserved for user interaction: systemd must not restart indefinitely.
failureExitCode :: String -> Int
failureExitCode category
  | category `elem` ["authentication-required", "authentication-failed", "token-refresh-failed", "Token file invalid"] = 78
  | otherwise = 1
