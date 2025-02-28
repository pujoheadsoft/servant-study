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
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import qualified Architecture.TaglessFinal.Gateway.UserGateway as UserGateway
import qualified Architecture.TaglessFinal.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.TaglessFinal.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.TaglessFinal.Gateway.NotificationGatewayPort as NotificationGatewayPort

import qualified Architecture.TaglessFinal.Usecase.UserPort as UserPort
import qualified Architecture.TaglessFinal.Usecase.NotificationPort as NotificationPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Api.Configuration (NotificationApiSettings)
import Control.Monad.Trans (lift)

handleSaveUserRequest
  :: NotificationApiSettings
  -> ConnectionPool
  -> UnvalidatedUser
  -> NotificationSettings
  -> Bool
  -> Handler String
handleSaveUserRequest notificationApiSettings pool user notificationSettings withNotify = do
  liftIO $ flip runSqlPool pool do
    sqlBackEnd <- ask
     
    runApp
      (execute user notificationSettings withNotify)
      AppEnv {sqlBackEnd = sqlBackEnd, notificationApiSettings = notificationApiSettings }

    pure "OK"
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

runApp :: AppM a -> AppEnv -> ReaderT SqlBackend IO a
runApp app env = liftIO $ runReaderT app env

instance UserPort.UserPort AppM where
  saveUser = UserGateway.saveUser
  saveNotificationSettings = UserGateway.saveNotificationSettings

instance NotificationPort.NotificationPort AppM where
  sendNotification = NotificationGateway.sendNotification

instance UserGatewayPort.UserGatewayPort AppM where
  saveUser user = do
    env <- ask
    lift $ runReaderT (UserDriver.saveUser user) env.sqlBackEnd
    
  saveNotificationSettings userNotification = do
    env <- ask
    lift $ runReaderT (UserDriver.saveNotificationSettings userNotification) env.sqlBackEnd

instance NotificationGatewayPort.NotificationGatewayPort AppM where
  sendNotification message = do
    env <- ask
    NotificationDriver.postMessage env.notificationApiSettings message

data AppEnv = AppEnv {
  sqlBackEnd :: SqlBackend,
  notificationApiSettings :: NotificationApiSettings
}

type AppM = ReaderT AppEnv IO

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e