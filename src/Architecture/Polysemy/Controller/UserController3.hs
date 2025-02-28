{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-
  runSqlPoolもエフェクトに処理させるやつ
-}
module Architecture.Polysemy.Controller.UserController3 where

import Control.Monad.Reader (ReaderT)
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Polysemy (makeSem, Sem, interpret, embed, Member, runM, reinterpret, Embed)
import Database.Persist.Sql (ConnectionPool)

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import Architecture.Polysemy.Usecase.SaveUser (execute)

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)

import Polysemy.Error (runError)
import qualified Architecture.Polysemy.Usecase.UserPort as UserPort
import qualified Architecture.Polysemy.Usecase.NotificationPort as NotificationPort
import qualified Architecture.Polysemy.Gateway.UserGateway as UserGateway
import qualified Architecture.Polysemy.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as NotificationGatewayPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Api.Configuration (NotificationApiSettings)
import Control.Monad.IO.Class (MonadIO, liftIO)

data DB m a where
  RunDB :: ReaderT SqlBackend IO a -> DB m a

makeSem ''DB

-- 中身を取り出すだけ
handleDb :: Sem '[DB] a -> ReaderT SqlBackend IO a
handleDb sem = runM $ reinterpret (\(RunDB action) -> embed action) sem

runPoolSql :: Member (Embed IO) r => ConnectionPool -> Sem '[DB] a -> Sem r a
runPoolSql pool sem = do
  let
    program = handleDb sem
  liftIO $ putStrLn "トランザクション開始"
  let x = runSqlPool program pool
  liftIO $ putStrLn "トランザクション終了"
  embed x

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Bool -> Handler String
handleSaveUserRequest notificationApiSettings pool user notificationSettings withNotify = do
  liftIO $ run pool notificationApiSettings user notificationSettings withNotify >>= either
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
  
run :: ConnectionPool -> NotificationApiSettings -> UnvalidatedUser -> NotificationSettings -> Bool -> IO (Either EmailError ())
run pool notificationApiSettings user notificationSettings withNotify =
  runM
  . runPoolSql pool
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
runUserPort = interpret \case
  UserPort.SaveUser user -> UserGateway.saveUser user
  UserPort.SaveNotificationSettings userId notification -> UserGateway.saveNotificationSettings userId notification

runNotificationPort :: Member NotificationGatewayPort.NotificationGatewayPort r => Sem (NotificationPort.NotificationPort : r) a -> Sem r a
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runUserGatewayPort
  :: Member DB r
  => Sem (UserGatewayPort.UserGatewayPort : r) a
  -> Sem r a
runUserGatewayPort = interpret \case
  UserGatewayPort.SaveUser user -> runDB $ UserDriver.saveUser user
  UserGatewayPort.SaveNotificationSettings notification -> runDB $ UserDriver.saveNotificationSettings notification

runNotificationGatewayPort
  :: Member DB r
  => NotificationApiSettings
  -> Sem (NotificationGatewayPort.NotificationGatewayPort : r) a -> Sem r a
runNotificationGatewayPort settings = interpret \case
  NotificationGatewayPort.SendNotification message -> runDB $ NotificationDriver.postMessage settings message
