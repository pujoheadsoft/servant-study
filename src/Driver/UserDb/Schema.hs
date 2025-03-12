

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Driver.UserDb.Schema where
import Database.Persist.TH (share, mkPersist, sqlSettings, persistLowerCase)
import Data.Int (Int32)
import Data.Text (Text)

share [mkPersist sqlSettings] [persistLowerCase|
User sql=users
  userId Int32
  firstName Text
  lastName Text
  email Text
  Primary userId
  deriving Show Eq

UserProfile sql=user_profiles
  userId Int32
  bio Text
  age Int32
  githubId Text
  Primary userId
  deriving Show Eq
|]