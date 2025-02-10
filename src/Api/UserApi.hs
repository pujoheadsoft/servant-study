{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.UserApi where
import Servant
import Data.Aeson
import Data.Aeson.TH
import Domain.User (UnvalidatedUser(..), UnvalidatedUserData (..), UserName (..), UserId (UserId), NotificationSettings (..))
import Domain.Email (UnvalidatedEmail(UnvalidatedEmail))
import GHC.Generics (Generic)
import Architecture.Simple.Controller.UserController (handleSaveUserRequest)

type USER_API =
  "v1" :> "users" :>
  (    Get '[JSON] [UserResponse]  -- GET: /v1/users
  :<|> Capture "userId" Integer
       :> QueryParam "architecture" String
       :> ReqBody '[JSON] UserRequest
       :> Put '[PlainText] String  -- PUT: /v1/users/${userId}?architecture=simple
  )

getUser :: Handler [UserResponse]
getUser = return 
  [ UserResponse 1 "Isaac" "Newton"
  , UserResponse 2 "Albert" "Einstein"
  ]

putUser :: Integer -> Maybe String -> UserRequest -> Handler String
putUser userId architecture request = do
  let 
    user = UnvalidatedUser
      { userId = UserId $ fromIntegral userId
      , userData = UnvalidatedUserData
        { name = UserName
          { first = request.name.first
          , last = request.name.last
          }
        , email = UnvalidatedEmail request.email
        }
      }
    notificationSettings = NotificationSettings
      { email = request.notifications.email
      , push = request.notifications.push
      }

  handleSaveUserRequest user notificationSettings

  return "OK"


data UserRequest = UserRequest
  { name          :: UserNameRequest
  , email         :: String
  , notifications :: NotificationRequest
  } deriving (Eq, Show, Generic)

data UserNameRequest = UserNameRequest
  { first :: String
  , last  :: String
  } deriving (Eq, Show, Generic)

data NotificationRequest = NotificationRequest
  { email :: Bool
  , push  :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON NotificationRequest
instance FromJSON UserNameRequest
instance FromJSON UserRequest

data UserResponse = UserResponse
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserResponse)