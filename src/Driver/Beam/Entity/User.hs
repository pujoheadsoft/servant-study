{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Driver.Beam.Entity.User where

import Database.Beam
import Data.Text
import Data.Int (Int32)

data UserT f = User
  {
    user_id :: Columnar f Int32,
    first_name :: Columnar f Text,
    last_name :: Columnar f Text,
    email :: Columnar f Text
  } deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey :: UserT column -> PrimaryKey UserT column
  primaryKey x = UserId x.user_id