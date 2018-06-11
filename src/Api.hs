{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , api
    ) where

import           Domain  (Contract, User)
import           RIO
import           Servant ((:<|>), (:>), Capture, Get, JSON, Proxy (..))


type API =
            "users" :> Get '[JSON] [User]
    :<|>    "user" :> Capture "id" Int :> Get '[JSON] User
    :<|>    "contracts" :> Get '[JSON] [Contract]


api :: Proxy API
api = Proxy



{-

"/users" -> List Users
"/user/:id" -  User

"/contracts" -> List Contracts
"/contract/address/:address" -> Contract
"/contract/id/:id" -> Contract





-}
