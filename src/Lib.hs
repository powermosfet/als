module Lib
  ( als
  ) where

import Protolude

import Api (server, api)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import qualified Config
import qualified Data.Text as T
import qualified Outlook
import qualified Outlook.Auth

data Error a
  = ConfigError Config.Error
  | OutlookError Outlook.Error
  | AuthError Outlook.Auth.Error
  deriving (Show)

als :: IO ()
als = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    result <- runExceptT $
      case args of
        ( "--auth" : _ ) -> do
          print "Reauthenticating"
          withExceptT AuthError Outlook.Auth.authenticate

        _ -> do
          port <- withExceptT ConfigError $ Config.findOption "PORT" Config.int
          lists <- withExceptT OutlookError $ Outlook.getLists
          liftIO $ do
              putStrLn ("Starting ALS api server, listening on port " ++ (show port))
              mapM print lists
              run port (serve api server)

    case result of
        Left error -> print error

        Right _ -> return ()
