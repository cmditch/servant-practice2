{-# LANGUAGE ImplicitParams #-}

module DB where

import           Database.Persist.Postgresql (ConnectionString)
import           RIO
-- import           Control.Monad.Reader        (runReaderT)
-- import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger        (LogStr, MonadLogger (..))
import qualified Control.Monad.Logger        as LegacyLogger
import           Database.Persist            (Entity, Key (..), SelectOpt (..),
                                              get, selectList, toPersistValue,
                                              (<.), (==.))
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, toSqlKey,
                                              withPostgresqlConn)
import qualified GHC.Stack                   as GS
import           Model
import           System.Log.FastLogger       (fromLogStr)


connString :: ConnectionString
connString =
    "host=127.0.0.1 port=5432 user=test dbname=opaleye password=qwerty"


runAction :: (MonadIO m, MonadUnliftIO m, MonadLogger m ) => ConnectionString -> SqlPersistT m a -> m a
runAction connectionString action =
    withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend


migrateDB :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => ConnectionString -> m ()
migrateDB connString = runAction connString (runMigration migrateAll)


getUser :: (MonadIO m) => Int -> SqlPersistT m (Maybe User)
getUser = get . toSqlKey . fromIntegral

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
