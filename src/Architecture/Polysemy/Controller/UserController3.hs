{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-
  runSqlPoolもエフェクトに処理させるやつ
-}
module Architecture.Polysemy.Controller.UserController3 where

import Api.Configuration (NotificationApiSettings)
import Architecture.Polysemy.Gateway.NotificationGateway qualified as NotificationGateway
import Architecture.Polysemy.Gateway.NotificationGatewayPort qualified as NotificationGatewayPort
import Architecture.Polysemy.Gateway.UserGateway qualified as UserGateway
import Architecture.Polysemy.Gateway.UserGatewayPort qualified as UserGatewayPort
import Architecture.Polysemy.Usecase.NotificationPort qualified as NotificationPort
import Architecture.Polysemy.Usecase.SaveUser (execute)
import Architecture.Polysemy.Usecase.UserPort qualified as UserPort
import Common.Logger (logError)
import Control.Exception (SomeException (..))
import Control.Exception.Safe (Exception (toException), MonadCatch, MonadThrow, catches, throw, throwM)
import Control.Exception.Safe qualified as Ex
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Typeable
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (ConnectionPool)
import Domain.Email (EmailError (InvalidEmailFormat))
import Domain.User (UserProfile, UnvalidatedUser)
import Driver.Api.NotificationApiDriverReq qualified as NotificationDriver
import Driver.UserDb.UserDriver qualified as UserDriver
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem, reinterpret, runM)
import Polysemy.Error (Error, catch, runError)
import Servant (Handler, ServerError (errBody), err400, err500, throwError)

data DB m a where
  RunDB :: ReaderT SqlBackend IO a -> DB m a

makeSem ''DB

-- 中身を取り出すだけ
handleDb :: Sem '[DB] a -> ReaderT SqlBackend IO a
handleDb sem = runM $ reinterpret (\(RunDB action) -> embed action) sem

runPoolSql :: (Member (Embed IO) r) => ConnectionPool -> Sem '[DB] a -> Sem r a
runPoolSql pool sem = do
  let program = handleDb sem
  liftIO $ putStrLn "トランザクション開始"
  let x = runSqlPool program pool
  liftIO $ putStrLn "トランザクション終了"
  embed x

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> UserProfile -> Bool -> Handler String
handleSaveUserRequest notificationApiSettings pool user profile withNotify =
  do
    liftIO $
      run pool notificationApiSettings user profile withNotify
        >>= either
          Ex.throw -- 外側のハンドラに任せる
          (const $ pure "OK")
    `catches` [ Ex.Handler $ \(InvalidEmailFormat e) -> do
                  logError e
                  throwError $ err400 {errBody = pack e},
                Ex.Handler $ \(SomeException e) -> do
                  logError e
                  throwError $ err500 {errBody = pack $ show e}
              ]

run :: ConnectionPool -> NotificationApiSettings -> UnvalidatedUser -> UserProfile -> Bool -> IO (Either EmailError ())
run pool notificationApiSettings user profile withNotify =
  runM
    . runPoolSql pool
    . runError
    . runUserGatewayPort
    . runUserPort
    . runNotificationGatewayPort notificationApiSettings
    . runNotificationPort
    $ execute user profile withNotify

runUserPort :: (Member UserGatewayPort.UserGatewayPort r) => Sem (UserPort.UserPort : r) a -> Sem r a
runUserPort = interpret \case
  UserPort.SaveUser user -> UserGateway.saveUser user
  UserPort.SaveProfile userId notification -> UserGateway.saveProfile userId notification

runNotificationPort :: (Member NotificationGatewayPort.NotificationGatewayPort r) => Sem (NotificationPort.NotificationPort : r) a -> Sem r a
runNotificationPort = interpret \case
  NotificationPort.SendNotification message -> NotificationGateway.sendNotification message

runUserGatewayPort ::
  (Member DB r) =>
  Sem (UserGatewayPort.UserGatewayPort : r) a ->
  Sem r a
runUserGatewayPort = interpret \case
  UserGatewayPort.SaveUser user -> runDB $ UserDriver.saveUser user
  UserGatewayPort.SaveProfile notification -> runDB $ UserDriver.saveProfile notification

runNotificationGatewayPort ::
  (Member DB r) =>
  NotificationApiSettings ->
  Sem (NotificationGatewayPort.NotificationGatewayPort : r) a ->
  Sem r a
runNotificationGatewayPort settings = interpret \case
  NotificationGatewayPort.SendNotification message -> runDB $ NotificationDriver.postMessage settings message

-- ----------------------------------------------------------------

data AuthError = InvalidCredentials | TokenExpired
  deriving (Show)

instance Exception AuthError

data FileError = FileNotFound FilePath | PermissionDenied FilePath
  deriving (Show)

instance Exception FileError

class (MonadThrow m) => UnwrapEither e m a where
  unwrapEither :: e -> m a

-- Either e a をアンラップするインスタンス
instance (MonadThrow m, Exception e) => UnwrapEither (Either e a) m a where
  unwrapEither (Left e) = throwM e
  unwrapEither (Right a) = return a

-- 末端の型をそのまま返すインスタンス
instance (MonadThrow m) => UnwrapEither a m a where
  unwrapEither = return

-- ネストされたEitherを再帰的にアンラップするインスタンス
instance {-# OVERLAPPABLE #-} (MonadThrow m, Exception e, UnwrapEither b m a) => UnwrapEither (Either e b) m a where
  unwrapEither (Left e) = throwM e
  unwrapEither (Right b) = unwrapEither b

toHandler :: Sem '[Error AuthError, Error FileError, Embed IO] a -> Handler a
toHandler sem =
  do
    result <- liftIO $ runM . runError @FileError . runError @AuthError $ sem

    unwrapEither result
    `catches` [ Ex.Handler $ \(e :: AuthError) -> do
                  logError e
                  throwError $ err400 {errBody = "AuthError: " <> pack (show e)},
                Ex.Handler $ \(e :: FileError) -> do
                  logError e
                  throwError $ err500 {errBody = "FileError: " <> pack (show e)}
              ]