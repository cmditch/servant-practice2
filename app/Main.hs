{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

-- Packages
import qualified Network.HTTP.Types.Status  as Http
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           Options.Applicative.Simple
import qualified Paths_servant_practice2
import           RIO
import qualified RIO.HashMap                as HM
import           RIO.Process
import           Servant


-- Internal
import           Api                        (API)
import qualified Api
import qualified DB
import           Model                      (Contract (..), User (..))


-- TYPES
------------------------------------------------------------
data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  -- Add other app-specific configuration information here
  }

instance Show App where
  show n = show $ appOptions n

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  } deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> switch ( long "verbose" <> short 'v' <> help "Verbose output?")


instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })



-- MAIN
------------------------------------------------------------


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_servant_practice2.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    optionsParser
    empty

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run




-- RIO APP
------------------------------------------------------------



run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  appEnv <- ask
  logDebug $ (<>) "Your App env is: " $ displayShow appEnv
  Api.writeSwaggerJSON
  DB.migrateDB DB.connString
  liftIO $ startApp $ appLogFunc appEnv


startApp :: LogFunc -> IO ()
startApp logFunc =
  Warp.runSettings (warpSettings logFunc) (waiApp logFunc)


warpSettings :: LogFunc -> Warp.Settings
warpSettings logFunc =
  Warp.setLogger (toWarpLogger logFunc) Warp.defaultSettings
  & Warp.setPort 8000


toWarpLogger :: LogFunc -> (Wai.Request -> Http.Status -> Maybe Integer -> IO ())
toWarpLogger logFunc = \req status mFileSize ->  runRIO logFunc $ do
  logDebug $ "App received request: " <> displayShow req
  logDebug $ "Http Status: " <> displayShow status
  logDebug $ "File Size: " <> displayShow mFileSize



waiApp :: LogFunc -> Wai.Application
waiApp logFunc = Servant.serve Api.api (server logFunc)



-- Servant handlers
--------------------------------
server :: LogFunc -> Server API
server logFunc = users
            :<|> userById
            :<|> contracts
  where
      users :: Servant.Handler [User]
      users = return (HM.elems usersTable)

      userById :: Int -> Servant.Handler User
      userById uid = do
        mUser <- liftIO $ runRIO logFunc $ DB.runAction DB.connString (DB.getUser uid)
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

