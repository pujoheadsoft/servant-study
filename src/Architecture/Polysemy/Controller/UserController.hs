{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Controller.UserController where

import Domain.User (UnvalidatedUser, UserProfile)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.Postgresql (runSqlPool, ConnectionPool, SqlBackend)
import Control.Monad.IO.Class (liftIO)
import Architecture.Polysemy.Usecase.SaveUser (execute)
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
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Api.Configuration (NotificationApiSettings)
import Common.Logger (logError)

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> UserProfile -> Bool -> Handler String
handleSaveUserRequest notificationApiSettings pool user profile withNotify = do
  liftIO $ flip runSqlPool pool do
    run notificationApiSettings user profile withNotify >>= either
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

run :: NotificationApiSettings -> UnvalidatedUser -> UserProfile -> Bool -> ReaderT SqlBackend IO (Either EmailError ())
run notificationApiSettings user profile withNotify =
  runM
  . runError
  . runUserGatewayPort
  . runUserPort
  . runNotificationGatewayPort notificationApiSettings
  . runNotificationPort
  $ execute user profile withNotify

runUserPort :: Member UserGatewayPort.UserGatewayPort r => Sem (UserPort.UserPort : r) a -> Sem r a
runUserPort = interpret \case
  UserPort.SaveUser user -> UserGateway.saveUser user
  UserPort.SaveProfile userId notification -> UserGateway.saveProfile userId notification

runNotificationPort :: Member NotificationGatewayPort.NotificationGatewayPort r => Sem (NotificationPort.NotificationPort : r) a -> Sem r a
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runUserGatewayPort :: Member (Embed (ReaderT SqlBackend IO)) r => Sem (UserGatewayPort.UserGatewayPort : r) a -> Sem r a
runUserGatewayPort = interpret \case
  UserGatewayPort.SaveUser user -> embed $ UserDriver.saveUser user
  UserGatewayPort.SaveProfile notification -> embed $ UserDriver.saveProfile notification

runNotificationGatewayPort
  :: Member (Embed (ReaderT SqlBackend IO)) r
  => NotificationApiSettings
  -> Sem (NotificationGatewayPort.NotificationGatewayPort : r) a -> Sem r a
runNotificationGatewayPort settings = interpret \case
  NotificationGatewayPort.SendNotification message -> embed $ NotificationDriver.postMessage settings message
