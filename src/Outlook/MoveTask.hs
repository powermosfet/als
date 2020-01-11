{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.MoveTask
    ( MoveTask(..)
    ) where

import Protolude

import Data.Aeson (ToJSON)
import Data.ByteString as B
import GHC.Generics (Generic)

data MoveTask = MoveTask
    { parentFolderId :: [Char]
    } deriving (Show, Generic, ToJSON)
