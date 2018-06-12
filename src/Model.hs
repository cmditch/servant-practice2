{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


module Model where

import qualified Data.Aeson          as Aeson
import           Database.Persist    (Entity (..))
import qualified Database.Persist.TH as PTH
import           RIO

-- USER
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|

  User sql=users
    firstName Text
    lastName Text
    deriving Show Read Eq Generic

  Contract sql=contracts
    userId Int
    address Text
    deriving Show Read Eq Generic

|]


instance Aeson.FromJSON User
instance Aeson.ToJSON User
instance Aeson.FromJSON Contract
instance Aeson.ToJSON Contract
