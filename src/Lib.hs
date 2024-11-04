{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics (Generic)

{-
  仕様
  ユーザー情報を登録する
  入力はJsonで受け取る
  出力はusersテーブルとuser_notificationsテーブルへの書き込み
  テーブルへの書き込みが失敗したらロールバックする
  メールアドレスをチェックするドメインロジックがある
  ログ出力を行う
-}

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "v1" :> "systems" :> "ping" :> Get '[PlainText] String
      :<|> "v1" :> "users" :> Get '[JSON] [User]
      :<|> "v1" :> "simple" :> "users" :> Capture "userId" Integer
                                       :> ReqBody '[JSON] UserRequest
                                       :> Put '[PlainText] String

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = ping 
    :<|> getUser
    :<|> putUser

ping :: Handler String
ping = return "pong"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

getUser :: Handler [User]
getUser = return users

putUser :: Integer -> UserRequest -> Handler String
putUser _ _ = return "OK"


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
