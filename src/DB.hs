{-# LANGUAGE ImplicitParams #-}

module DB where

import           RIO
import qualified RIO.Text                    as Text

import           Data.Aeson                  ((.:))
import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.Types            as JSON (Parser, typeMismatch)

import qualified System.Etc                  as Etc

import           Control.Monad.Logger        (LogStr, MonadLogger (..))
import qualified Control.Monad.Logger        as LegacyLogger
import qualified GHC.Stack                   as GS
import           System.Log.FastLogger       (fromLogStr)

import           Database.Persist            (Entity, Key (..), SelectOpt (..),
                                              get, selectList, toPersistValue,
                                              (<.), (==.))
import           Database.Persist.Postgresql (ConnectionString,
                                              withPostgresqlConn)
import           Database.Persist.Sql        (SqlBackend, SqlPersistT,
                                              runMigration, toSqlKey)
import           Database.Persist.Sqlite     (withSqliteConn)

import           Model



------------------------------------------------------------
-- Configuration

data DBDriver
    = Sqlite Text
    | Postgres ConnectionString


buildDBDriver :: Etc.Config -> IO DBDriver
buildDBDriver config = do
    connString <- Etc.getConfigValueWith parseConnString ["database"] config
    driver <- Etc.getConfigValueWith (parseDriver connString) ["database", "driver"] config
    return driver


parseDriver :: ConnectionString -> JSON.Value -> JSON.Parser DBDriver
parseDriver connString =
    JSON.withText "DB.Driver" $ \handleName ->
        case Text.toLower handleName of
            "postgres" -> return (Postgres connString)
            "sqlite"   -> return (Sqlite $ decodeUtf8With lenientDecode connString)
            _          -> JSON.typeMismatch "Database Driver" (JSON.String handleName)


parseConnString :: JSON.Value -> JSON.Parser ConnectionString
parseConnString = JSON.withObject "DB.ConnectionString" $ \obj -> do
    user     <- obj .: "username"
    password <- obj .: "password"
    database <- obj .: "database"
    host     <- obj .: "host"
    port     <- obj .: "port"
    return $
        Text.encodeUtf8
        $ Text.unwords [ "user=" <> user
                        , "password=" <> password
                        , "dbname=" <> database
                        , "host=" <> host
                        , "port=" <> port
                        ]





------------------------------------------------------------
-- Runners

runAction :: (MonadIO m, MonadUnliftIO m, MonadLogger m ) => DBDriver -> SqlPersistT m a -> m a
runAction dbRunner action =
    let
        runner backend =
            runReaderT action backend
    in
        case dbRunner of
            Sqlite connectionText ->
                withSqliteConn connectionText runner

            Postgres connectionByteString ->
                withPostgresqlConn connectionByteString runner



migrateDB :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => DBDriver -> m ()
migrateDB dbRunner =
    runAction dbRunner (runMigration migrateAll)


getUser :: (MonadIO m) => Int -> SqlPersistT m (Maybe User)
getUser =
    get . toSqlKey . fromIntegral




-----------------------------------------------------------------------------------------------
-- Temporary
-- This is in the upcoming release of RIO Orphans
-- Manually being added here for now, so it can play nice with Persistent-postgres

instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog loc source level msg =
      let ?callStack = GS.fromCallSiteList [("", GS.SrcLoc
            { GS.srcLocPackage = LegacyLogger.loc_package loc
            , GS.srcLocModule = LegacyLogger.loc_module loc
            , GS.srcLocFile = LegacyLogger.loc_filename loc
            , GS.srcLocStartLine = fst $ LegacyLogger.loc_start loc
            , GS.srcLocStartCol = snd $ LegacyLogger.loc_start loc
            , GS.srcLocEndLine = fst $ LegacyLogger.loc_end loc
            , GS.srcLocEndCol = snd $ LegacyLogger.loc_end loc
            })]
       in logGeneric source rioLogLevel (display $ LegacyLogger.toLogStr msg)
    where
      rioLogLevel =
        case level of
          LegacyLogger.LevelDebug      -> LevelDebug
          LegacyLogger.LevelInfo       -> LevelInfo
          LegacyLogger.LevelWarn       -> LevelWarn
          LegacyLogger.LevelError      -> LevelError
          LegacyLogger.LevelOther name -> LevelOther name


instance Display LogStr where
  display = displayBytesUtf8 . fromLogStr
