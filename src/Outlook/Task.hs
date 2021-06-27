{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.Task
    ( Task(..)
    , Id
    , increment
    , matchesTitle
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type Id = [Char]

data Task = Task
    { id :: Id
    , title :: Text
    , status :: Text
    } deriving (Show, Generic, ToJSON, FromJSON)

matchesTitle :: Text -> Task -> Bool
matchesTitle titleToMatch task =
    title task == titleToMatch

increment :: Task -> Task
increment task =
    task { status = "notStarted" }
