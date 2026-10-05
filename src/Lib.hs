{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (als) where
import Prelude
import Control.Concurrent (myThreadId)
import Control.Exception
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Posix.Signals
import qualified GraphClient as G
import qualified SetupAuth
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
    ["--auth"] -> SetupAuth.authenticate >>= either failStartup (const (logEvent "authentication-complete"))
    [] -> do
      cfg <- loadConfig >>= either failStartup pure
      tokens <- loadTokens (tokenFile cfg) >>= either failStartup pure
      client <- G.newClient cfg G.production tokens
      logEvent "worker-starting"
      runWorker cfg client
    _ -> failStartup "Usage: als [--auth]")
    (\(e :: AsyncException) -> case e of
      UserInterrupt -> logEvent "worker-stopped"
      _ -> throwIO e)
  case result of
    Left e -> do
      case fromException e of
        Just (WorkerFailure category) -> logEvent category
        Nothing -> logEvent "worker-failed"
      exitFailure
    Right () -> pure ()
 where failStartup message = throwIO (WorkerFailure message)
