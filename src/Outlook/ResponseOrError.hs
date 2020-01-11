{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Outlook.ResponseOrError
    ( ResponseOrError(..)
    , OutlookApiError(..)
    ) where

import Protolude

import Control.Monad (fail)
import Data.Aeson (ToJSON, FromJSON, parseJSON, Value(..))
import GHC.Generics (Generic)
import qualified Data.HashMap.Lazy as HashMap

data ResponseOrError a 
    = SuccessResponse a
    | ErrorResponse OutlookApiError
    deriving (Show)

instance (FromJSON a) => FromJSON (ResponseOrError a) where
  parseJSON (value@(Object v)) =
    let
        entry = head (HashMap.toList v)
    in
    case entry of
        Just ("error", _) -> ErrorResponse <$> parseJSON value
        _ -> SuccessResponse <$> parseJSON value

  parseJSON _ = fail "Invalid JSON type"

data OutlookApiError = OutlookApiError
    { error :: InnerOutlookError
    } deriving (Show, Generic, FromJSON)

data InnerOutlookError = InnerOutlookError
    { code :: [Char]
    , message :: [Char]
    } deriving (Show, Generic, FromJSON)
