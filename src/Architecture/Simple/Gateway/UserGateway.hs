{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Architecture.Simple.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.Beam.UserDriver as UserDriver
import qualified Driver.Beam.Entity.User as E
import qualified Driver.Beam.Entity.UserNotification as N
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), NotificationSettings (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)

updateUser :: (MonadThrow m, MonadIO m) => User -> m ()
updateUser (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = E.User (fromIntegral userId) (pack first) (pack last) (pack email.value)
  UserDriver.updateUser u

updateNotificationSettings :: (MonadThrow m, MonadIO m) => UserId -> NotificationSettings -> m ()
updateNotificationSettings (UserId userId) (NotificationSettings email push) = do
  let n = N.UserNotification (fromIntegral userId) email push
  UserDriver.updateNotificationSettings n
