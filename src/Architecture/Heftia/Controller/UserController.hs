-- トランザクションに乗せてSQLを実行するのに runSqlPoolを直接使う版
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Architecture.Heftia.Controller.UserController where

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.Postgresql (runSqlPool, ConnectionPool, SqlBackend)
import Control.Monad.IO.Class (liftIO)
import Architecture.Heftia.Usecase.SaveUser (execute)
import qualified Architecture.Heftia.Usecase.UserPort as UserPort
import qualified Architecture.Heftia.Gateway.UserGateway as UserGateway
import qualified Architecture.Heftia.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.Heftia.Usecase.NotificationPort as NotificationPort
import qualified Architecture.Heftia.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.Heftia.Gateway.NotificationGatewayPort as NotificationGatewayPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Control.Monad.Reader (ReaderT)
import Control.Monad.Hefty (type (<|), (:!!), type (~>), interpret, send, runEff, translate)
import Control.Monad.Hefty.Except (runThrow)
import Api.Configuration (NotificationApiSettings)
import Common.Logger (logError)

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> Handler String
handleSaveUserRequest apiSetting pool user notificationSettings withNotify = do
  liftIO $ flip runSqlPool pool do
    run apiSetting user notificationSettings withNotify >>= either
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
run apiSetting user notificationSettings withNotify =
  runEff
  . runThrow
  . runUserGatewayPort
  . runUserPort
  . runNotificationGatewayPort apiSetting
  . runNotificationPort
  $ execute user notificationSettings withNotify

runUserPort :: (UserGatewayPort.UserGatewayPort <| r) => eh :!! UserPort.UserPort ': r ~> eh :!! r
runUserPort = interpret \case
  UserPort.SaveUser user -> UserGateway.saveUser user
  UserPort.SaveNotificationSettings userId notification -> UserGateway.saveNotificationSettings userId notification

runNotificationPort
  :: (NotificationGatewayPort.NotificationGatewayPort <| r)
  => eh :!! NotificationPort.NotificationPort ': r ~> eh :!! r
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runUserGatewayPort :: (ReaderT SqlBackend IO <| r) => eh :!! UserGatewayPort.UserGatewayPort ': r ~> eh :!! r
runUserGatewayPort = interpret \case
  UserGatewayPort.SaveUser user -> send $ UserDriver.saveUser @IO user
  UserGatewayPort.SaveNotificationSettings notification -> send $ UserDriver.saveNotificationSettings @IO notification

runNotificationGatewayPort
  :: (ReaderT SqlBackend IO <| r)
  => NotificationApiSettings
  -> eh :!! NotificationGatewayPort.NotificationGatewayPort ': r ~> eh :!! r
runNotificationGatewayPort apiSetting = translate go
  where
    go :: NotificationGatewayPort.NotificationGatewayPort a -> ReaderT SqlBackend IO a
    go = \case
      NotificationGatewayPort.SendNotification message -> NotificationDriver.postMessage apiSetting message
