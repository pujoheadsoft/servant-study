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
import Database.Persist.Postgresql (ConnectionPool)
import qualified Architecture.Simple.Controller.UserController as Simple
import qualified Architecture.TaglessFinal.Controller.UserController as TaglessFinal
import qualified Architecture.Polysemy.Controller.UserController as Polysemy
import qualified Architecture.Heftia.Controller.UserController as Heftia
import Control.Exception (throw)
import Data.Maybe (fromMaybe)

data Architecture
  = Simple
  | TaglessFinal
  | Polysemy
  | Heftia

type USER_API =
  "v1" :> "users" :>
  (    Get '[JSON] [UserResponse]  -- GET: /v1/users
  :<|> Capture "userId" Integer
       :> QueryParam "architecture" Architecture
       :> QueryParam "withNotify" Bool
       :> ReqBody '[JSON] UserRequest
       :> Put '[PlainText] String  -- PUT: /v1/users/${userId}?architecture=simple
  )

getUser :: ConnectionPool -> Handler [UserResponse]
getUser _ = pure
  [ UserResponse 1 "Isaac" "Newton"
  , UserResponse 2 "Albert" "Einstein"
  ]

putUser :: ConnectionPool -> Integer -> Maybe Architecture -> Maybe Bool -> UserRequest -> Handler String
putUser pool userId architecture _withNotify request = do
  let 
    user = toUnvalidatedUser userId request
    notificationSettings = toNotificationSettings request
    withNotify = fromMaybe False _withNotify

  case architecture of
    Just Simple -> Simple.handleSaveUserRequest pool user notificationSettings
    Just TaglessFinal -> TaglessFinal.handleSaveUserRequest pool user notificationSettings
    Just Polysemy -> Polysemy.handleSaveUserRequest pool user notificationSettings
    Just Heftia -> Heftia.handleSaveUserRequest2 pool user notificationSettings
    Nothing -> throw $ err400 { errBody = "Missing architecture query parameter" }


toUnvalidatedUser :: Integer -> UserRequest -> UnvalidatedUser
toUnvalidatedUser userId request = UnvalidatedUser
  { userId = UserId $ fromIntegral userId
  , userData = UnvalidatedUserData
    { name = UserName
      { first = request.name.first
      , last = request.name.last
      }
    , email = UnvalidatedEmail request.email
    }
  }

toNotificationSettings :: UserRequest -> NotificationSettings
toNotificationSettings request = NotificationSettings
  { email = request.notifications.email
  , push = request.notifications.push
  }

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

instance FromHttpApiData Architecture where
  parseQueryParam "simple" = Right Simple
  parseQueryParam "taglessFinal" = Right TaglessFinal
  parseQueryParam "polysemy" = Right Polysemy
  parseQueryParam "heftia" = Right Heftia
  parseQueryParam _ = Left "Invalid architecture query parameter"