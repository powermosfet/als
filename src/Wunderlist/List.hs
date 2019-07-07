{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wunderlist.List
    ( List(..)
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data List = List
    { id :: Int
    , created_at :: Text
    , title :: Text
    -- , listType :: Text
    -- , type_ :: Text
    , revision :: Int
    } deriving (Show, Generic, FromJSON)

