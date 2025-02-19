{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Controller.UserController2 where

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
import Architecture.Polysemy.Usecase.SaveUser (execute)

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)

import Polysemy (runM, Member, Embed, Sem, interpret, embed)
import Polysemy.Error (runError)
import Control.Monad.Reader (ReaderT)
import qualified Architecture.Polysemy.Usecase.UserPort as UserPort
import qualified Architecture.Polysemy.Usecase.NotificationPort as NotificationPort
import qualified Architecture.Polysemy.Gateway.UserGateway as UserGateway
import qualified Architecture.Polysemy.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as NotificationGatewayPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriver as NotificationDriver
import Api.Configuration (NotificationApiSettings)

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> Handler String
handleSaveUserRequest notificationApiSettings pool user notificationSettings withNotify = do
  liftIO $ flip runSqlPool pool do
    run notificationApiSettings user notificationSettings withNotify >>= either
      Ex.throw -- 外側のハンドラに任せる
      \_ -> pure "OK"
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

run :: NotificationApiSettings -> UnvalidatedUser -> NotificationSettings -> Bool -> ReaderT SqlBackend IO (Either EmailError ())
run notificationApiSettings user notificationSettings withNotify =
  runM
  . runError
  . runUserGatewayPort
  . runUserPort
  . runNotificationGatewayPort notificationApiSettings
  . runNotificationPort
  $ execute user notificationSettings withNotify

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e

runUserPort :: Member UserGatewayPort.UserGatewayPort r => Sem (UserPort.UserPort : r) a -> Sem r a
runUserPort = UserGateway.runUserPort UserGateway.createUserPortFunctions

runNotificationPort :: Member NotificationGatewayPort.NotificationGatewayPort r => Sem (NotificationPort.NotificationPort : r) a -> Sem r a
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runUserGatewayPort :: Member (Embed (ReaderT SqlBackend IO)) r => Sem (UserGatewayPort.UserGatewayPort : r) a -> Sem r a
runUserGatewayPort = interpret \case
  UserGatewayPort.SaveUser user -> embed $ UserDriver.saveUser user
  UserGatewayPort.SaveNotificationSettings notification -> embed $ UserDriver.saveNotificationSettings notification

runNotificationGatewayPort
  :: Member (Embed (ReaderT SqlBackend IO)) r
  => NotificationApiSettings
  -> Sem (NotificationGatewayPort.NotificationGatewayPort : r) a -> Sem r a
runNotificationGatewayPort settings = interpret \case
  NotificationGatewayPort.SendNotification message -> embed $ NotificationDriver.postMessage settings message
