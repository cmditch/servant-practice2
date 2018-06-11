{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , api
    ) where


import           Domain  (User)
import           Servant ((:>), Get, JSON, Proxy (..))


type API =
    ( "users" :> Get '[JSON] [User]
    )

api :: Proxy API
api = Proxy
