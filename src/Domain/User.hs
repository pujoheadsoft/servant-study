module Domain.User where
import Domain.Email (Email)

data User = User 
  { userId        :: UserId
  , name          :: UserName
  , email         :: Email
  } deriving (Eq, Show)

newtype UserId = UserId Int
  deriving (Eq, Show)

data UserName = UserName
  { first :: String
  , last  :: String
  } deriving (Eq, Show)