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
import Architecture.Heftia.Usecase.SaveUser (execute, UserUsecasePort(..))

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import qualified Architecture.Heftia.Gateway.UserGateway as Gateway
import qualified Architecture.Heftia.Gateway.UserGatewayPort as GatewayPort
import qualified Driver.UserDb.UserDriver as Driver
import Control.Monad.Reader (ReaderT)
import Control.Monad.Hefty (type (<|), (:!!), type (~>), type (<<:), interpret, send, runEff, Type,  makeEffectF, translate, type (<<|))
import Control.Monad.Hefty.Except (runThrow, runThrowIO)
import Data.Effect.Except (Catch, withExcept)

data RunSql m (a :: Type) where
  RunSql :: ReaderT SqlBackend m a -> RunSql m a
makeEffectF [''RunSql]

runRunSql :: (IO <| r) => ConnectionPool -> eh :!! RunSql IO ': r ~> eh :!! r
runRunSql pool = interpret \case
  RunSql a -> liftIO $ runSqlPool a pool

-- runSqlPoolを直接使う版
handleSaveUserRequest :: ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Handler String
handleSaveUserRequest pool user notificationSettings = do
  liftIO $ flip runSqlPool pool do
    run user notificationSettings >>= either
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

-- runSqlPoolもエフェクトに処理させる版
handleSaveUserRequest2 :: ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Handler String
handleSaveUserRequest2 pool user notificationSettings = do
  (do
    liftIO $ run2 pool user notificationSettings
    pure "OK")
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

run :: UnvalidatedUser -> NotificationSettings -> ReaderT SqlBackend IO (Either EmailError ())
run user notificationSettings =
  runEff
  . runThrow
  . runGatewayPort
  . runUsecasePort
  $ execute user notificationSettings

run2 :: ConnectionPool -> UnvalidatedUser -> NotificationSettings -> IO ()
run2 pool user notification = do
    runEff
  . runThrowIO @EmailError  
  . runRunSql pool
  . runGatewayPort2
  . runUsecasePort
  $ execute user notification

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e

runUsecasePort :: (GatewayPort.UserGatewayPort <| r) => eh :!! UserUsecasePort ': r ~> eh :!! r
runUsecasePort = interpret \case
  SaveUser user -> Gateway.saveUser user
  SaveNotificationSettings userId notification -> Gateway.saveNotificationSettings userId notification

runGatewayPort :: (ReaderT SqlBackend IO <| r) => eh :!! GatewayPort.UserGatewayPort ': r ~> eh :!! r
runGatewayPort = interpret \case
  GatewayPort.SaveUser user -> send $ Driver.saveUser @IO user
  GatewayPort.SaveNotificationSettings notification -> send $ Driver.saveNotificationSettings @IO notification

runGatewayPort2 :: (RunSql IO <| ef) => eh :!! GatewayPort.UserGatewayPort ': ef ~> eh :!! ef
runGatewayPort2 = translate go
  where
    go :: GatewayPort.UserGatewayPort a -> RunSql IO a
    go = \case
      GatewayPort.SaveUser user -> RunSql $ Driver.saveUser @IO user
      GatewayPort.SaveNotificationSettings notification -> RunSql $ Driver.saveNotificationSettings @IO notification