{-# LANGUAGE NoImplicitPrelude #-}
module Delivery where
import Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

newtype Item = Item { description :: T.Text } deriving (Eq, Show)
instance FromJSON Item where
  parseJSON = withObject "item" $ \o -> do
    d <- o .: "description"
    if T.null (T.strip d) then fail "blank description" else pure (Item d)
parseItem :: BL.ByteString -> Either String Item
parseItem = eitherDecode

data Failure = Retry String Int | InvalidItem | Fatal String deriving (Eq, Show)
data DeliveryOps = DeliveryOps
  { create :: T.Text -> IO (Either Failure ())
  , acknowledge :: IO (), reject :: IO ()
  , delay :: Int -> IO (), event :: String -> IO ()
  }

-- Exceptions (including shutdown) propagate without acknowledging the delivery.
processDelivery :: DeliveryOps -> BL.ByteString -> IO (Either String ())
processDelivery ops body = case parseItem body of
  Left _ -> event ops "message-invalid" >> reject ops >> pure (Right ())
  Right item -> loop (description item)
 where
  loop title = do
    result <- create ops title
    case result of
      Right () -> acknowledge ops >> event ops "task-created" >> pure (Right ())
      Left InvalidItem -> event ops "item-rejected" >> reject ops >> pure (Right ())
      Left (Fatal category) -> event ops category >> pure (Left category)
      Left (Retry category seconds) -> event ops category >> delay ops seconds >> loop title
