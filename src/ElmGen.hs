{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ElmGen where

import           RIO

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)

import qualified Api
import qualified Model        as M

instance ElmType M.Contract
instance ElmType M.User

spec :: Spec
spec = Spec ["OppAPI"]
             ( defElmImports
             : toElmTypeSource    (Proxy :: Proxy M.User)
             : toElmDecoderSource (Proxy :: Proxy M.User)
             : toElmTypeSource    (Proxy :: Proxy M.Contract)
             : toElmDecoderSource (Proxy :: Proxy M.Contract)
             : generateElmForAPI  (Proxy :: Proxy Api.API)
             )

generate :: RIO env ()
generate = liftIO $ specsToDir [spec] "elm"
