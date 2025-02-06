{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller.UserController where
import Servant
import Database.Beam
import Data.Aeson
import Data.Aeson.TH
import Usecase.Simple.UpdateUser
import Domain.User (UnvalidatedUser(..), UnvalidatedUserData (..), UserName (..), UserId (UserId))
import Domain.Email (UnvalidatedEmail(UnvalidatedEmail))

type USER_API = "v1" :> 
  (    "users" :> Get '[JSON] [UserResponse]
  :<|> "simple" :> "users" :> Capture "userId" Integer
                           :> ReqBody '[JSON] UserRequest
                           :> Put '[PlainText] String
  )

getUser :: Handler [UserResponse]
getUser = return 
  [ UserResponse 1 "Isaac" "Newton"
  , UserResponse 2 "Albert" "Einstein"
  ]

putUser :: Integer -> UserRequest -> Handler String
putUser userId request = do
  let user = UnvalidatedUser
        { userId = UserId $ fromIntegral userId
        , userData = UnvalidatedUserData
          { name = UserName
            { first = request.name.first
            , last = request.name.last
            }
          , email = UnvalidatedEmail request.email
          }
        }
  liftIO $ execute user
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