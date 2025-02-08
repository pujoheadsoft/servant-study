module Domain.User where
import Domain.Email (Email, UnvalidatedEmail)
import Control.Lens (makeLenses)

newtype UserId = UserId { value :: Int }
  deriving (Eq, Show)

data UserName = UserName
  { first :: String
  , last  :: String
  } deriving (Eq, Show)
makeLenses ''UserName

data UserData = UserData
  { name :: UserName
  , email :: Email
  } deriving (Eq, Show)
makeLenses ''UserData

data UnvalidatedUserData = UnvalidatedUserData
  { name :: UserName
  , email :: UnvalidatedEmail
  } deriving (Eq, Show)
makeLenses ''UnvalidatedUserData

data NotificationSettings = NotificationSettings
  { email :: Bool
  , push  :: Bool
  } deriving (Eq, Show)
makeLenses ''NotificationSettings

-- 新規ユーザー(検証済み)
type ValidatedNewUser = UserData

-- 新規ユーザー(未検証)
type UnvalidatedNewUser = UnvalidatedUserData

-- 既存のユーザー(検証済み)
data User = User 
  { userId   :: UserId
  , userData :: UserData
  } deriving (Eq, Show)
makeLenses ''User

-- 既存のユーザー(未検証)
data UnvalidatedUser = UnvalidatedUser
  { userId :: UserId
  , userData :: UnvalidatedUserData
  } deriving (Show, Eq)
makeLenses ''UnvalidatedUser
