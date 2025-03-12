{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Driver.UserDb.UserDriver where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Driver.UserDb.Schema (User(..), Key (UserKey, UserProfileKey), UserProfile(..))
import Database.Persist.Sql (SqlBackend, repsert)
import Control.Monad.Reader (ReaderT)

saveUser :: MonadIO m => User -> ReaderT SqlBackend m ()
saveUser user = do
  liftIO $ putStrLn "ユーザ情報保存"
  repsert (UserKey user.userUserId) user

saveProfile :: MonadIO m => UserProfile -> ReaderT SqlBackend m ()
saveProfile profile = do
  liftIO $ putStrLn "Profile保存"
  repsert (UserProfileKey profile.userProfileUserId) profile
