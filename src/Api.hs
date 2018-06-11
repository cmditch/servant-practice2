{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Api
    ( API
    , api
    , writeSwaggerJSON
    ) where

import qualified Data.Aeson          as Aeson
import           Domain              (Contract (..), User (..))
import           RIO
import           RIO.ByteString.Lazy (toStrict)
import           Servant             ((:<|>), (:>), Capture, Get, JSON,
                                      Proxy (..))
-- import qualified Servant.API  as SA
-- import qualified Servant.Docs as SD
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



-- Servant Doc Generators

-- instance SD.ToCapture (Capture "id" Int) where
--     toCapture _ =
--         SD.DocCapture "id" "The Users id"

-- instance SD.ToSample User where
--     toSamples _ = SD.singleSample (User 1 "Satoshi" "Nakamoto")

-- instance SD.ToSample Contract where
--     toSamples _ = SD.singleSample (Contract 1 1 "Satoshi")

instance ToSchema User where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "This user is some kinda user"
      & mapped.schema.example ?~ Aeson.toJSON (User 1 "Satoshi" "Nakamoto")


instance ToSchema Contract where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "This is a sweet contract"
        & mapped.schema.example ?~ Aeson.toJSON (Contract 1 1 "0x12313123132")


apiDocs :: Swagger
apiDocs = toSwagger api
    & info.title        .~ "Servant/Swagger Test Api"
    & info.(Data.Swagger.version)      .~ "1.0"
    & info.description  ?~ "This is an API that tests servant + swagger docs"
    & info.license      ?~ "mmk now"


writeSwaggerJSON :: RIO env ()
writeSwaggerJSON =  writeFileBinary "swagger.json" (toStrict $ Aeson.encode apiDocs)


{-

"/users" -> List Users
"/user/:id" -  User

"/contracts" -> List Contracts
"/contract/address/:address" -> Contract
"/contract/id/:id" -> Contract

-}
