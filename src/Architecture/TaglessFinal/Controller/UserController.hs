{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Architecture.TaglessFinal.Controller.UserController where

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex 
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool, ConnectionPool, SqlBackend)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Architecture.TaglessFinal.Usecase.SaveUser (execute)

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Architecture.TaglessFinal.Gateway.UserGateway as Gateway
import qualified Architecture.TaglessFinal.Gateway.UserGatewayPort as GatewayPort
import qualified Driver.UserDb.UserDriver as Driver
import Architecture.TaglessFinal.Usecase.UserPort

handleSaveUserRequest :: ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Handler String
handleSaveUserRequest pool user notificationSettings = do
  liftIO $ flip runSqlPool pool do
    execute user notificationSettings
    pure "OK"
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

instance UserPort (ReaderT SqlBackend IO) where
  saveUser = Gateway.saveUser
  saveNotificationSettings = Gateway.saveNotificationSettings

instance GatewayPort.UserGatewayPort (ReaderT SqlBackend IO) where
  saveUser = Driver.saveUser
  saveNotificationSettings = Driver.saveNotificationSettings
  
-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e