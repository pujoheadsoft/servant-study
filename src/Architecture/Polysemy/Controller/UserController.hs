{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Controller.UserController where

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
import Architecture.Polysemy.Usecase.SaveUser (execute, UserUsecasePort(..))

import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import qualified Architecture.Polysemy.Gateway.UserGateway as Gateway
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as GatewayPort
import qualified Driver.UserDb.UserDriver as Driver
import Polysemy (runM, Member, Embed, Sem, interpret, embed)
import Polysemy.Error (runError)
import Control.Monad.Reader (ReaderT)

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

run :: UnvalidatedUser -> NotificationSettings -> ReaderT SqlBackend IO (Either EmailError ())
run user notificationSettings = 
  runM 
  . runError
  . runGatewayPort
  . runUsecasePort 
  $ execute user notificationSettings

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e

runUsecasePort :: Member GatewayPort.UserGatewayPort r => Sem (UserUsecasePort : r) a -> Sem r a
runUsecasePort = interpret \case
  SaveUser user -> Gateway.saveUser user
  SaveNotificationSettings userId notification -> Gateway.saveNotificationSettings userId notification

runGatewayPort :: Member (Embed (ReaderT SqlBackend IO)) r => Sem (GatewayPort.UserGatewayPort : r) a -> Sem r a
runGatewayPort = interpret \case
  GatewayPort.SaveUser user -> embed $ Driver.saveUser user
  GatewayPort.SaveNotificationSettings notification -> embed $ Driver.saveNotificationSettings notification

