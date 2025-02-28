{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Architecture.Heftia.Controller.UserController2 where

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
import Control.Monad.Hefty (type (<|), (:!!), type (~>), interpret, send, runEff,  makeEffectF, translate, transform, reinterpret)
import Control.Monad.Hefty.Except (runThrow)
import Api.Configuration (NotificationApiSettings)

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e

-- runSqlPoolをエフェクトに処理させる版
data RunSql a where
  RunSql :: ReaderT SqlBackend IO a -> RunSql a
makeEffectF [''RunSql]

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> Handler String
handleSaveUserRequest apiSetting pool user notificationSettings withNotify = do
  liftIO $ run apiSetting pool user notificationSettings withNotify >>= either
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

run :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> IO (Either EmailError ())
run apiSetting pool user notification withNotify  = do
    runEffPoolSqlWithTransaction pool
  . runThrow @EmailError  
  . runGatewayPort
  . runUserPort
  . runNotificationGatewayPort apiSetting
  . runNotificationPort
  $ execute user notification withNotify

runUserPort :: (UserGatewayPort.UserGatewayPort <| r) => eh :!! UserPort.UserPort ': r ~> eh :!! r
runUserPort = interpret \case
  UserPort.SaveUser user -> UserGateway.saveUser user
  UserPort.SaveNotificationSettings userId notification -> UserGateway.saveNotificationSettings userId notification

runNotificationPort
  :: (NotificationGatewayPort.NotificationGatewayPort <| r)
  => eh :!! NotificationPort.NotificationPort ': r ~> eh :!! r
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runGatewayPort :: RunSql <| ef => eh :!! UserGatewayPort.UserGatewayPort ': ef ~> eh :!! ef
runGatewayPort = translate go
  where
    go :: UserGatewayPort.UserGatewayPort a -> RunSql a
    go = \case
      UserGatewayPort.SaveUser user -> RunSql $ UserDriver.saveUser @IO user
      UserGatewayPort.SaveNotificationSettings notification -> RunSql $ UserDriver.saveNotificationSettings @IO notification

runNotificationGatewayPort
  :: RunSql <| ef
  => NotificationApiSettings
  -> eh :!! NotificationGatewayPort.NotificationGatewayPort ': ef ~> eh :!! ef
runNotificationGatewayPort apiSetting = translate go
  where
    go :: NotificationGatewayPort.NotificationGatewayPort a -> RunSql a
    go = \case
      NotificationGatewayPort.SendNotification message -> RunSql $ NotificationDriver.postMessage apiSetting message

extract :: '[] :!! '[RunSql] ~> ReaderT SqlBackend IO
extract = runEff . transform (\(RunSql action) -> action)

runPoolSqlWithTransaction :: IO <| ef => ConnectionPool -> '[] :!! '[RunSql] ~> '[] :!! ef
runPoolSqlWithTransaction pool ef = do
  let
    program = extract ef
  liftIO $ putStrLn "トランザクション開始"
  let x = runSqlPool program pool
  liftIO $ putStrLn "トランザクション終了"
  send x

runEffPoolSqlWithTransaction :: MonadIO m => ConnectionPool -> '[] :!! '[RunSql] ~> m
runEffPoolSqlWithTransaction pool ef = liftIO . runEff $ runPoolSqlWithTransaction pool ef

-- 
runPoolSql :: IO <| ef => ConnectionPool -> eh :!! RunSql ': ef ~> eh :!! ef
runPoolSql pool = interpret \case
  RunSql action -> do
    liftIO $ putStrLn "トランザクション開始"
    let x = runSqlPool action pool
    liftIO $ putStrLn "トランザクション終了"
    send x
