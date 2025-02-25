{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Driver.UserDb.UserDriver where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Driver.UserDb.Schema (User(..), Key (UserKey, UserNotificationKey), UserNotification(..))
import Database.Persist.Sql (SqlBackend, repsert)
import Control.Monad.Reader (ReaderT)

saveUser :: MonadIO m => User -> ReaderT SqlBackend m ()
saveUser user = do
  liftIO $ putStrLn "ユーザ保存"
  repsert (UserKey user.userUserId) user

saveNotificationSettings :: MonadIO m => UserNotification -> ReaderT SqlBackend m ()
saveNotificationSettings notification = do
  liftIO $ putStrLn "通知情報保存"
  repsert (UserNotificationKey notification.userNotificationUserId) notification
