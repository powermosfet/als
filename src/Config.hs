module Config 
    ( Config(..)
    , Error(..)
    , fromEnv
    ) where

import Protolude

import Control.Error.Util (failWithM, failWith)
import System.Environment (lookupEnv)
import Wunderlist (Auth(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

data Config = Config
    { auth :: Auth
    , listName :: Text
    } deriving (Show)

data Error
    = NoEnvVar Text
    | ParseErrorInt
    deriving (Show)

bytestring :: (Monad m) => [Char] -> ExceptT Error m ByteString
bytestring = return . B.pack

text :: (Monad m) => [Char] -> ExceptT Error m Text
text = return . T.pack

int :: (Monad m) => [Char] -> ExceptT Error m Int
int = failWith ParseErrorInt . readMaybe

fromEnv :: ExceptT Error IO (Config, Int)
fromEnv = 
    let
        findOption name convert =
            lookupEnv name
                & failWithM (NoEnvVar (T.pack name))
                >>= convert
    in do
    auth <- return Auth
        <*> findOption "CLIENT_ID" bytestring
        <*> findOption "ACCESS_TOKEN" bytestring

    config <- return (Config auth)
        <*> findOption "LIST_NAME" text

    return ((,) config)
        <*> findOption "PORT" int
