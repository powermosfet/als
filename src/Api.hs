{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( AlsApi
    , server
    , api
    ) where

import Protolude 

import Config (Config(..))
import Control.Error.Util (failWith)
import Data.Aeson (FromJSON)
import Servant ((:>), JSON, ServantErr, errBody, err404, err500, Post, Proxy(..), ReqBody)
import Servant.Server (Handler)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as Http
import qualified Wunderlist
import qualified Wunderlist.CreateTask as CreateTask
import qualified Wunderlist.List as List
import qualified Wunderlist.Task as WT


data Item = Item
    { title :: Text
    } deriving (Show, Generic, FromJSON)

data Error
  = WunderlistError Wunderlist.Error
  | ListNotFound
  deriving (Show)

type AlsApi = "item" :> ReqBody '[JSON] Item :> Post '[JSON] WT.Task

server :: Config -> Item -> Handler WT.Task
server = postItem

postItem :: Config -> Item -> Handler WT.Task
postItem (Config {..}) item = do
    result <- liftIO $ runExceptT $ do
        dagligvarer  <- Wunderlist.getLists auth
            &   withExceptT WunderlistError
            <&> filter (\l -> List.title l == listName)
            <&> head
            >>= failWith ListNotFound 
        Wunderlist.createTask auth (CreateTask.CreateTask
            { list_id = List.id dagligvarer
            , title = (title item)
            }) & withExceptT WunderlistError
    case result of
        Right task -> return task

        Left e -> throwError (makeServantError e)

makeServantError :: Error -> ServantErr
makeServantError (WunderlistError (Wunderlist.HttpException (Http.HttpExceptionRequest _ _))) =
    err500
makeServantError (WunderlistError (Wunderlist.HttpException (Http.InvalidUrlException url _))) =
    err500
        { errBody = BL.fromStrict $ B.pack $ "Error parsing url: " ++ url
        }
makeServantError ListNotFound =
    err404

api :: Proxy AlsApi
api = Proxy
