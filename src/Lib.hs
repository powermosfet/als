module Lib
  ( someFunc
  ) where

import Protolude

import Control.Error.Util (failWith)
import qualified Config
import qualified Wunderlist

data AlsError
  = CreateConfigError Config.CreateConfigError
  | GetListsError Wunderlist.GetListsError
  | ListNotFound
  deriving (Show)

someFunc :: IO ()
someFunc = do
    result <- runExceptT $ do
        config <- withExceptT CreateConfigError
            $ Config.fromEnv
        let auth = Config.toAuth config
        dagligvarerLookup  <- withExceptT GetListsError (Wunderlist.getLists auth)
            <&> filter (\l -> Wunderlist.title l == (Config.listName config))
            <&> head
        dagligvarer <- failWith ListNotFound dagligvarerLookup
        liftIO $ print dagligvarer

    case result of
        Left error -> print error

        Right _ -> return ()
