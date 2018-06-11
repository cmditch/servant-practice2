{-# LANGUAGE DeriveGeneric #-}

module Domain (User(..)) where

import qualified Data.Aeson as Aeson
import           RIO

data User = User
  { userId        :: !Int
  , userFirstName :: !String
  , userLastName  :: !String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON User
instance Aeson.ToJSON User
