{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.UserApi where
import Servant
import Data.Aeson
import Data.Aeson.TH
import Domain.User (UnvalidatedUser(..), UnvalidatedUserData (..), UserName (..), UserId (UserId), UserProfile (..))
import Domain.Email (UnvalidatedEmail(UnvalidatedEmail))
import GHC.Generics (Generic)
import Database.Persist.Postgresql (ConnectionPool)
import qualified Architecture.Simple.Controller.UserController as Simple
import qualified Architecture.TaglessFinal.Controller.UserController as TaglessFinal
import qualified Architecture.Polysemy.Controller.UserController as Polysemy
import qualified Architecture.Polysemy.Controller.UserController3 as Polysemy3
import qualified Architecture.Heftia.Controller.UserController as Heftia
import qualified Architecture.Heftia.Controller.UserController2 as Heftia2
import Control.Exception (throw)
import Data.Maybe (fromMaybe)
import Api.Configuration (ApiSettings(..))
import Data.Text (Text)

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

putUser :: ApiSettings -> ConnectionPool -> Integer -> Maybe Architecture -> Maybe Bool -> UserRequest -> Handler String
putUser apiSettings pool userId architecture _withNotify request = do
  let 
    user = toUnvalidatedUser userId request
    profile = toProfile request
    withNotify = fromMaybe False _withNotify

  case architecture of
    Just Simple -> Simple.handleSaveUserRequest apiSettings.notification pool user profile withNotify
    Just TaglessFinal -> TaglessFinal.handleSaveUserRequest apiSettings.notification pool user profile withNotify
    Just Polysemy -> Polysemy3.handleSaveUserRequest apiSettings.notification pool user profile withNotify
    Just Heftia -> Heftia2.handleSaveUserRequest apiSettings.notification pool user profile withNotify
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

toProfile :: UserRequest -> UserProfile
toProfile request = UserProfile
  { bio = request.profile.bio
  , age = request.profile.age
  , githubId = request.profile.githubId
  }

data UserRequest = UserRequest
  { name          :: UserNameRequest
  , email         :: String
  , profile       :: UserProfileRequest
  } deriving (Eq, Show, Generic)

data UserNameRequest = UserNameRequest
  { first :: Text
  , last  :: Text
  } deriving (Eq, Show, Generic)

data UserProfileRequest = UserProfileRequest
  { bio :: Text
  , age :: Int
  , githubId :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserProfileRequest
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