

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

UserNotification sql=user_notifications
  userId Int32
  emailNotifications Bool
  pushNotifications Bool
  Primary userId
  deriving Show Eq
|]
