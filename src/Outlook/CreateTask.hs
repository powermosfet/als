{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.CreateTask
    ( CreateTask(..)
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data CreateTask = CreateTask
    { subject :: Text
    } deriving (Show, Generic, ToJSON)
