{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Driver.Beam.Entity.UserNotification where

import Database.Beam

data UserNotificationT f = User
  {
    userId :: Columnar f Int,
    emailNotifications :: Columnar f Bool,
    pushNotifications :: Columnar f Bool
  } deriving Generic

type UserNotification = UserNotificationT Identity
type UserId = PrimaryKey UserNotificationT Identity

deriving instance Show UserNotification
deriving instance Eq UserNotification

instance Beamable UserNotificationT
instance Table UserNotificationT where
  data PrimaryKey UserNotificationT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . userId