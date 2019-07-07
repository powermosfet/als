{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wunderlist.Task
    ( Task(..)
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Task = Task
    { id :: Int
    , created_at :: Text
    , created_by_id :: Int
    , list_id :: Int
    , revision :: Int
    , starred :: Bool
    , completed :: Bool
    , title :: Text
    } deriving (Show, Generic, FromJSON, ToJSON)
