{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TokenStore where
import Prelude
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO
import System.Posix.Files (setFileMode, getSymbolicLinkStatus)
import System.IO.Error (isDoesNotExistError)

data Tokens = Tokens { access_token :: T.Text, refresh_token :: T.Text }
  deriving (Eq, Generic)
instance Show Tokens where show _ = "Tokens <redacted>"
instance FromJSON Tokens
instance ToJSON Tokens

validTokens :: Tokens -> Bool
validTokens t = all (not . T.null . T.strip) [access_token t, refresh_token t]

loadTokens :: Maybe FilePath -> IO (Either String Tokens)
loadTokens Nothing = environmentTokens
loadTokens (Just file) = do
  exists <- try (getSymbolicLinkStatus file)
  case exists of
    Left e | isDoesNotExistError e -> environmentTokens
           | otherwise -> pure (Left "Token file unreadable")
    Right _ -> do
      result <- try (BS.readFile file)
      pure $ case (result :: Either IOException BS.ByteString) of
        Left _ -> Left "Token file unreadable"
        Right body -> case eitherDecodeStrict' body of
          Left _ -> Left "Token file invalid"
          Right t | validTokens t -> Right t
                  | otherwise -> Left "Token file invalid"

environmentTokens :: IO (Either String Tokens)
environmentTokens = do
  a <- lookupEnv "ACCESS_TOKEN"
  r <- lookupEnv "REFRESH_TOKEN"
  pure $ case (a,r) of
    (Just x,Just y) | validTokens (Tokens (T.pack x) (T.pack y)) -> Right (Tokens (T.pack x) (T.pack y))
    _ -> Left "Missing or invalid ACCESS_TOKEN / REFRESH_TOKEN"

-- The temporary file is in the same directory so rename is atomic.
-- openBinaryTempFile creates mode 0600; set it explicitly before writing too.
saveTokens :: FilePath -> Tokens -> IO (Either String ())
saveTokens file tokens = do
  result <- try $ bracketOnError (openBinaryTempFile (takeDirectory file) ".als-tokens")
    (\(tmp,h) -> do
      catch (hClose h) (\(_ :: IOException) -> pure ())
      catch (removeFile tmp) (\(_ :: IOException) -> pure ()))
    (\(tmp,h) -> do
      setFileMode tmp 0o600
      BL.hPut h (encode tokens)
      hFlush h
      hClose h
      renameFile tmp file)
  pure $ case (result :: Either IOException ()) of
    Left _ -> Left "Token persistence failed"
    Right () -> Right ()
