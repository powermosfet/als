{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wunderlist.CreateTask
    ( CreateTask(..)
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data CreateTask = CreateTask
    { list_id :: Int
    , title :: Text
    } deriving (Show, Generic, ToJSON)

