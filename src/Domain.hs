{-# LANGUAGE DeriveGeneric #-}

module Domain (User(..), Contract(..)) where

import qualified Data.Aeson as Aeson
import           RIO

data User = User
  { userId        :: !Int
  , userFirstName :: !Text
  , userLastName  :: !Text
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON User
instance Aeson.ToJSON User


data Contract = Contract
  { contractId      :: !Int
  , contractUserId  :: !Int
  , contractAddress :: !Text
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Contract
instance Aeson.ToJSON Contract
