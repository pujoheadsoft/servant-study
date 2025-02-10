{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Driver.Beam.UserDriver where

import Database.Beam.Postgres (runBeamPostgresDebug, SqlError (..), Pg)
import Driver.Beam.Config (connection)
import Database.Beam (runInsert, Table (primaryKey))
import Driver.Beam.Database (UserDb(..), userDb)
import Database.Beam.Query (insertValues)
import Driver.Beam.Entity.User (User)
import Driver.Beam.Entity.UserNotification (UserNotification)
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict(insertOnConflict, conflictingFields), onConflictUpdateAll)
import Control.Exception.Safe (MonadThrow, catch, throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (Exception)
import Data.ByteString.Char8 (unpack)
import Data.Functor (($>))

updateUser :: (MonadThrow m, MonadIO m) => User -> m ()
updateUser user = do
  liftIO (go `catch` errorHandler) $> ()
  where
    go = do
      conn <- connection
      runBeamPostgresDebug putStrLn conn $ do
        runInsert $
          insertOnConflict
            userDb.users
            (insertValues [user])
            (conflictingFields primaryKey)
            onConflictUpdateAll

updateNotificationSettings :: (MonadThrow m, MonadIO m) => UserNotification -> m ()
updateNotificationSettings notification = do
  liftIO (go `catch` errorHandler) $> ()
  where
    go = do
      conn <- connection
      runBeamPostgresDebug putStrLn conn $ do
        runInsert $
          insertOnConflict
            userDb.user_notifications
            (insertValues [notification])
            (conflictingFields primaryKey)
            onConflictUpdateAll

errorHandler :: (MonadIO m, MonadThrow m) => SqlError -> m a
errorHandler = go
  where
    go e = do
      (liftIO . print . show) e
      (throw . DriverError . unpack . sqlErrorMsg) e

-- これは移す
data DriverError = DriverError String
  deriving (Show, Eq)

instance Exception DriverError

{-
  こうすれば同じトランザクションに乗せられそう
  ただPg型が他のレイヤーに出てくるのは避けないといけない
  インタフェース的な何かを使えばいけそう
-}
execute :: (MonadThrow m, MonadIO m) => Pg a -> m ()
execute program = liftIO (go `catch` errorHandler) $> ()
  where
    go = do
      conn <- connection
      runBeamPostgresDebug putStrLn conn program

_updateUser :: User -> Pg ()
_updateUser user =
  runInsert $
    insertOnConflict
      userDb.users
      (insertValues [user])
      (conflictingFields primaryKey)
      onConflictUpdateAll

_updateNotificationSettings :: UserNotification -> Pg ()
_updateNotificationSettings notification =
  runInsert $
    insertOnConflict
      userDb.user_notifications
      (insertValues [notification])
      (conflictingFields primaryKey)
      onConflictUpdateAll

example :: (MonadThrow m, MonadIO m) => User -> UserNotification -> m ()
example user notifications = execute do
  _updateUser user
  _updateNotificationSettings notifications