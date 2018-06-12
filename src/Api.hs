{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , api
    , writeSwaggerJSON
    ) where

import qualified Data.Aeson          as Aeson
import           Model               (Contract (..), User (..))
import           RIO
import           RIO.ByteString.Lazy (toStrict)
import           Servant             ((:<|>), (:>), Capture, Get, JSON,
                                      Proxy (..))

import           Control.Lens
import           Data.Swagger
import           GHC.Generics
import           Servant
import           Servant.Swagger

type API =
            "users" :> Get '[JSON] [User]
    :<|>    "user" :> Capture "id" Int :> Get '[JSON] User
    :<|>    "contracts" :> Get '[JSON] [Contract]


api :: Proxy API
api = Proxy


instance ToSchema User where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "This user is some kinda user"
      & mapped.schema.example ?~ Aeson.toJSON (User "Satoshi" "Nakamoto")


instance ToSchema Contract where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "This is a sweet contract"
        & mapped.schema.example ?~ Aeson.toJSON (Contract 1 "0x12313123132")


apiDocs :: Swagger
apiDocs = toSwagger api
    & info.title        .~ "Servant/Swagger Test Api"
    & info.(Data.Swagger.version)      .~ "1.0"
    & info.description  ?~ "This is an API that tests servant + swagger docs"
    & info.license      ?~ "mmk now"


writeSwaggerJSON :: RIO env ()
writeSwaggerJSON =  writeFileBinary "swagger.json" (toStrict $ Aeson.encode apiDocs)
