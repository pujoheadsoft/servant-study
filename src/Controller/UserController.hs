{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Controller.UserController where
import Servant
import Database.Beam
import Data.Aeson
import Data.Aeson.TH
import Usecase.Simple.UpdateUser

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
putUser _ _ = do
  --liftIO execute
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