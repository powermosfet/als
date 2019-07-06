module Config 
    ( Config(..)
    , CreateConfigError(..)
    , fromEnv
    , toAuth
    ) where

import Protolude

import Control.Error.Util (failWithM)
import System.Environment (lookupEnv)
import Wunderlist (Auth(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

data Config = Config
    { clientId :: B.ByteString
    , accessToken :: B.ByteString
    , listName :: Text
    } deriving (Show)

data CreateConfigError
    = NoEnvVar Text
    deriving (Show)

fromEnv :: ExceptT CreateConfigError IO Config
fromEnv = do
    clientId <- lookupEnv "CLIENT_ID"
        & failWithM (NoEnvVar "CLIENT_ID")
        <&> B.pack
    accessToken <- lookupEnv "ACCESS_TOKEN"
        & failWithM (NoEnvVar "ACCESS_TOKEN")
        <&> B.pack
    listName <- lookupEnv "LIST_NAME"
        & failWithM (NoEnvVar "LIST_NAME")
        <&> T.pack
    return $ Config
        clientId
        accessToken
        listName

toAuth :: Config -> Auth
toAuth (Config {..}) = Auth 
    { clientId = clientId
    , accessToken = accessToken
    }
