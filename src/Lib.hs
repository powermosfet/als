module Lib
  ( als
  ) where

import Protolude

import qualified Config
import Api (server, api)
import Servant (serve)
import Network.Wai.Handler.Warp (run)

als :: IO ()
als = do
    args <- getArgs
    case args of
        [ "--auth" ] -> do
            clientId <- Config.findOption "CLIENT_ID" Config.bytestring
            let url =  "https://login.microsoftonline.com/common/oauth2/v2.0/authorize?client_id=" ++ clientId ++ "&scope=Tasks.ReadWrite&response_type=code"
            print url

        _ -> do
            result <- runExceptT $ do
                port <- Config.findOption "PORT" Config.int
                liftIO $ do
                    putStrLn ("Starting ALS api server, listening on port " ++ (show port))
                    run port (serve api server)

            case result of
                Left error -> print error

                Right _ -> return ()
