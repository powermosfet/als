{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.Task
    ( Task(..)
    , Id
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type Id = [Char]

data Task = Task
    { id :: Id
    } deriving (Show, Generic, ToJSON, FromJSON)
