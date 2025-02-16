{-# LANGUAGE FlexibleInstances #-}
module Domain.User where
import Domain.Email (Email, UnvalidatedEmail)
import Control.Lens (makeFieldsId)
import Data.Text (Text)

newtype UserId = UserId { value :: Int }
  deriving newtype (Eq, Show)
makeFieldsId ''UserId

data UserName = UserName
  { first :: Text
  , last  :: Text
  } deriving (Eq, Show)
makeFieldsId ''UserName

data UserData = UserData
  { name :: UserName
  , email :: Email
  } deriving (Eq, Show)
makeFieldsId ''UserData

data UnvalidatedUserData = UnvalidatedUserData
  { name :: UserName
  , email :: UnvalidatedEmail
  } deriving (Eq, Show)
makeFieldsId ''UnvalidatedUserData

data NotificationSettings = NotificationSettings
  { email :: Bool
  , push  :: Bool
  } deriving (Eq, Show)
makeFieldsId ''NotificationSettings

-- 新規ユーザー(検証済み)
type ValidatedNewUser = UserData

-- 新規ユーザー(未検証)
type UnvalidatedNewUser = UnvalidatedUserData

-- 既存のユーザー(検証済み)
data User = User 
  { userId   :: UserId
  , userData :: UserData
  } deriving (Eq, Show)
makeFieldsId ''User

-- 既存のユーザー(未検証)
data UnvalidatedUser = UnvalidatedUser
  { userId :: UserId
  , userData :: UnvalidatedUserData
  } deriving (Show, Eq)
makeFieldsId ''UnvalidatedUser
