{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.List
    ( List
    , Id
    ) where

import Protolude

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type Id = [Char]

data List = List
    { id :: Id
    , displayName :: [Char]
    } deriving (Show, Generic, ToJSON, FromJSON)
