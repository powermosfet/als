module Config 
    ( Error(..)
    , bytestring
    , findOption
    , int
    , string
    ) where

import Protolude

import Control.Error.Util (failWithM, failWith)
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

data Error
    = NoEnvVar Text
    | ParseErrorInt
    deriving (Show)

string :: [Char] -> ExceptT Error IO [Char]
string = return

bytestring :: [Char] -> ExceptT Error IO ByteString
bytestring = return . B.pack

text :: [Char] -> ExceptT Error IO Text
text = return . T.pack

int :: [Char] -> ExceptT Error IO Int
int = failWith ParseErrorInt . readMaybe

findOption :: [Char] -> ([Char] -> ExceptT Error IO a) -> ExceptT Error IO a
findOption name convert =
    lookupEnv name
        & failWithM (NoEnvVar (T.pack name))
        >>= convert
