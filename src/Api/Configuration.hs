{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Api.Configuration where
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

configuration :: IO AppSettings
configuration = loadYamlSettings ["./settings.yaml"] [] useEnv

data AppSettings = AppSettings
  { db :: DatabaseSettings,
    api :: ApiSettings
  }
  deriving (Eq, Show, Generic)

data DatabaseSettings = DatabaseSettings
  { host :: String
  , port :: Int
  , user :: String
  , password :: String
  , database :: String
  , poolsize :: Int
  }
  deriving (Eq, Show, Generic)

data ApiSettings = ApiSettings
  { notification :: NotificationApiSettings
  }
  deriving (Eq, Show, Generic)

data NotificationApiSettings = NotificationApiSettings
  { baseUrl :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON AppSettings
instance FromJSON DatabaseSettings
instance FromJSON ApiSettings
instance FromJSON NotificationApiSettings