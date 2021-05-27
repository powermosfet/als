{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Outlook.AuthToken
    ( AuthToken(..)
    ) where

import Protolude

import Data.Aeson (FromJSON)
import Data.ByteString as B
import GHC.Generics (Generic)

data AuthToken = AuthToken
    { access_token :: Text
    } deriving (Show, Generic, FromJSON)
