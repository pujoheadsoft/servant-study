{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Driver.Beam.Database where
import Database.Beam
import Driver.Beam.Entity.User
import Driver.Beam.Entity.UserNotification

data UserDb f = UserDb
  { users :: f (TableEntity UserT)
  , userNotifications :: f (TableEntity UserNotificationT)
  } deriving (Generic, Database be)

userDb :: DatabaseSettings be UserDb
userDb = defaultDbSettings