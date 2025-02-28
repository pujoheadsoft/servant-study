{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Architecture.Heftia.Controller.UserController where

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
import Architecture.Heftia.Usecase.SaveUser (execute)

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import qualified Architecture.Heftia.Usecase.UserPort as UserPort
import qualified Architecture.Heftia.Gateway.UserGateway as UserGateway
import qualified Architecture.Heftia.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.Heftia.Usecase.NotificationPort as NotificationPort
import qualified Architecture.Heftia.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.Heftia.Gateway.NotificationGatewayPort as NotificationGatewayPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Control.Monad.Reader (ReaderT)
import Control.Monad.Hefty (type (<|), (:!!), type (~>), interpret, send, runEff, Type,  makeEffectF, translate)
import Control.Monad.Hefty.Except (runThrow, runThrowIO, throw)
import Api.Configuration (NotificationApiSettings)

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e

-- runSqlPoolを直接使う版
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

-- runSqlPoolをエフェクトに処理させる版

data RunSql m (a :: Type) where
  RunSql :: ReaderT SqlBackend m a -> RunSql m a
makeEffectF [''RunSql]

-- これだとRunSqlが複数あった場合、別々のトランザクションになってしまう
runRunSql :: (IO <| r) => ConnectionPool -> eh :!! RunSql IO ': r ~> eh :!! r
runRunSql pool = interpret \case
  RunSql a -> liftIO do
    putStrLn "トランザクション開始"
    r <- runSqlPool a pool
    putStrLn "トランザクション終了"
    pure r

handleSaveUserRequest2 :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> Handler String
handleSaveUserRequest2 apiSetting pool user notificationSettings withNotify = do
  (do
    liftIO $ run2 apiSetting pool user notificationSettings withNotify
    pure "OK")
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

run2 :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> IO ()
run2 apiSetting pool user notification withNotify  = do
    runEff
  . runThrowIO @EmailError  
  . runRunSql pool
  . runGatewayPort2
  . runUserPort
  . runNotificationGatewayPort2 apiSetting
  . runNotificationPort
  $ execute user notification withNotify

runGatewayPort2 :: (RunSql IO <| ef) => eh :!! UserGatewayPort.UserGatewayPort ': ef ~> eh :!! ef
runGatewayPort2 = translate go
  where
    go :: UserGatewayPort.UserGatewayPort a -> RunSql IO a
    go = \case
      UserGatewayPort.SaveUser user -> RunSql $ UserDriver.saveUser @IO user
      UserGatewayPort.SaveNotificationSettings notification -> RunSql $ UserDriver.saveNotificationSettings @IO notification

runNotificationGatewayPort2
  :: (IO <| r)
  => NotificationApiSettings
  -> eh :!! NotificationGatewayPort.NotificationGatewayPort ': r ~> eh :!! r
runNotificationGatewayPort2 apiSetting = interpret \case
  NotificationGatewayPort.SendNotification message -> NotificationDriver.postMessage apiSetting message


