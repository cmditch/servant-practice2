{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

-- Packages
import qualified Paths_servant_practice2

import           RIO
import qualified RIO.HashMap                 as HM
import           RIO.Process
import qualified RIO.Text                    as Text

import qualified Network.HTTP.Types.Status   as Http
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant


import           Data.FileEmbed              (embedFile)
import qualified System.Etc                  as Etc

import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.Types            as JSON (Parser, typeMismatch)

import qualified Database.Persist.Postgresql as PG
import qualified Database.Persist.Sqlite     as Sqllite

import           Control.Monad.Logger        (LogStr, MonadLogger (..))

-- Internal
import           Api                         (API)
import qualified Api
import qualified DB
import           Model                       (Contract (..), User (..))


------------------------------------------------------------
-- App Structure

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appWebPort        :: !Warp.Port
  , appDBDriver       :: !DB.DBDriver
  -- Add other app-specific configuration information here
  }

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  } deriving (Show)


instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


------------------------------------------------------------
-- Configuration

specBytes :: ByteString
specBytes =
  $(embedFile "./config/spec.yaml")


parseConfigSpec :: MonadThrow m => m (Etc.ConfigSpec ())
parseConfigSpec =
  case decodeUtf8' specBytes of
    Left err     -> throwM err
    Right result -> Etc.parseConfigSpec result


resolveConfigSpec :: Etc.ConfigSpec () -> IO (Etc.Config, Vector SomeException)
resolveConfigSpec configSpec = do
  let
    defaultConfig = Etc.resolveDefault configSpec

  (fileConfig, fileWarnings) <- Etc.resolveFiles configSpec
  envConfig <- Etc.resolveEnv configSpec
  cliConfig <- Etc.resolvePlainCli configSpec

  return ( defaultConfig <> fileConfig <> envConfig <> cliConfig
          , fileWarnings
          )


buildConfig :: IO (Etc.Config, Vector SomeException)
buildConfig = do
  configSpec <- parseConfigSpec
  resolveConfigSpec configSpec


--------------------------------------------------------------------------------
-- Config Helpers

buildWebPort :: Etc.Config -> IO Warp.Port
buildWebPort config =
    Etc.getConfigValue ["webserver", "port"] config


buildLogOptions :: Etc.Config -> IO LogOptions
buildLogOptions config = do
    handle <- Etc.getConfigValueWith parseHandle ["logging", "handle"] config
    verbose <- Etc.getConfigValue ["logging", "verbose"] config
    logOptionsHandle handle verbose
  where
    parseHandle = JSON.withText "IO.Handle" $ \handleName ->
      case Text.toLower handleName of
        "stdout" -> return stdout
        "stderr" -> return stderr
        _        -> JSON.typeMismatch "IO.Handle" (JSON.String handleName)


------------------------------------------------------------
-- Main

main :: IO ()
main = do
  (config, _fileWarnings) <- buildConfig

  logOptions  <- buildLogOptions config
  dbDriver    <- DB.buildDBDriver config
  webPort     <- buildWebPort config
  pc          <- mkDefaultProcessContext

  withLogFunc logOptions $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appWebPort = webPort
          , appDBDriver = dbDriver
          }
     in runRIO app run



-- RIO APP
------------------------------------------------------------



run :: RIO App ()
run = do
  logDebug "Server starting..."
  appEnv <- ask
  logDebug $ "Using DB: " <> (displayShow $ appDBDriver appEnv)
  Api.writeSwaggerJSON
  DB.migrateDB $ appDBDriver appEnv
  startWebServer


startWebServer :: RIO App ()
startWebServer = do
  app <- ask
  let logFunc = appLogFunc app
      webport = appWebPort app
  waiApp' <- waiApp
  liftIO $ Warp.runSettings (warpSettings logFunc webport) waiApp'


warpSettings :: LogFunc -> Warp.Port -> Warp.Settings
warpSettings logFunc port =
  Warp.setLogger (toWarpLogger logFunc) Warp.defaultSettings
  & Warp.setPort port


toWarpLogger :: LogFunc -> (Wai.Request -> Http.Status -> Maybe Integer -> IO ())
toWarpLogger logFunc = \req status mFileSize ->  runRIO logFunc $ do
  logDebug $ "App received request: " <> displayShow req



waiApp :: RIO App (Wai.Application)
waiApp = do
  app <- ask
  return $ Servant.serve Api.api (server app)



-- Servant handlers
--------------------------------
server :: App -> Server API
server app = users
            :<|> userById
            :<|> contracts
    where
        logFunc = appLogFunc app
        dbDriver = appDBDriver app
        runDBAction = liftIO . runRIO logFunc . DB.runAction dbDriver

        users :: Servant.Handler [User]
        users = return (HM.elems usersTable)

        userById :: Int -> Servant.Handler User
        userById uid = do
            mUser <- runDBAction $ DB.getUser uid
            case mUser of
                Just user ->
                    return user
                Nothing ->
                    throwError err404 {errBody = "Could not find user by given id"}

        contracts :: Servant.Handler [Contract]
        contracts = return contractTable


usersTable :: HM.HashMap Int User
usersTable = HM.fromList
    [ (1, User "Satoshi" "Nakamoto")
    , (2, User "Haskell" "Curry")
    , (3, User "Alan" "Turing")
    ]

contractTable :: [Contract]
contractTable = []

