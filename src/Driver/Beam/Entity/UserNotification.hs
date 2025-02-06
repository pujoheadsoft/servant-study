{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Driver.Beam.Entity.UserNotification where

import Database.Beam
import Data.Int (Int32)

data UserNotificationT f = UserNotification
  {
    user_id :: Columnar f Int32,
    email_notifications :: Columnar f Bool,
    push_notifications :: Columnar f Bool
  } deriving Generic

type UserNotification = UserNotificationT Identity
type UserId = PrimaryKey UserNotificationT Identity

deriving instance Show UserNotification
deriving instance Eq UserNotification

instance Beamable UserNotificationT
instance Table UserNotificationT where
  data PrimaryKey UserNotificationT f = UserId (Columnar f Int32) deriving (Generic, Beamable)

  primaryKey :: UserNotificationT column -> PrimaryKey UserNotificationT column
  primaryKey x = UserId x.user_id