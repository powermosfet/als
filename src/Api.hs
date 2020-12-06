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

import Control.Error.Util (failWith)
import Data.Aeson (FromJSON)
import Outlook.CreateTask (CreateTask(..))
import Servant ((:>), JSON, errBody, err404, err500, Post, Proxy(..), ReqBody, ServerError)
import Servant.Server (Handler)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as Http
import qualified Outlook
import qualified Outlook.CreateTask as CreateTask 
import qualified Outlook.Task as WT


data Item = Item
    { title :: Text
    } deriving (Show, Generic, FromJSON)

data Error
  = OutlookError Outlook.Error
  deriving (Show)

type AlsApi = "item" :> ReqBody '[JSON] Item :> Post '[JSON] WT.Task

server :: Item -> Servant.Server.Handler WT.Task
server = postItem

postItem :: Item -> Servant.Server.Handler WT.Task
postItem  item = do
    result <- liftIO $ runExceptT $
        Outlook.createTask (CreateTask
            { subject = (title item)
            }) & withExceptT OutlookError
            
    case result of
        Right task -> return task

        Left e -> throwError (makeServantError e)

makeServantError :: Error -> ServerError
makeServantError (OutlookError (Outlook.HttpException (Http.HttpExceptionRequest _ _))) =
    err500
makeServantError (OutlookError (Outlook.HttpException (Http.InvalidUrlException url _))) =
    err500
        { errBody = BL.fromStrict $ B.pack $ "Error parsing url: " ++ url
        }
makeServantError err = 
    err500
        { errBody = BL.fromStrict $ B.pack $ show err
        }

api :: Proxy AlsApi
api = Proxy
