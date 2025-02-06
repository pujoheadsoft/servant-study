{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Gateway.Simple.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(..))
import qualified Driver.Beam.UserDriver as UserDriver
import qualified Driver.Beam.Entity.User as E
import qualified Driver.Beam.Entity.UserNotification as N
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), NotificationSettings (..))

updateUser :: User -> IO ()
updateUser (User (UserId userId) (UserData (UserName first last) (Email email))) = do
  let u = E.User (fromIntegral userId) (pack first) (pack last) (pack email)
  UserDriver.updateUser u

updateNotificationSettings :: UserId -> NotificationSettings -> IO ()
updateNotificationSettings (UserId userId) (NotificationSettings email push) = do
  let n = N.UserNotification (fromIntegral userId) email push
  UserDriver.updateNotificationSettings n