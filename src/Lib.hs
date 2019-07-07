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
    result <- runExceptT $ do
        (config, port) <- Config.fromEnv 
        liftIO $ do
            putStrLn ("Starting ALS api server, listening on port " ++ (show port))
            run port (serve api (server config))

    case result of
        Left error -> print error

        Right _ -> return ()
