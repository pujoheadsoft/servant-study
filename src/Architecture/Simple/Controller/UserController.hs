module Architecture.Simple.Controller.UserController where

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Architecture.Simple.Usecase.SaveUser (execute, UserUsecasePort(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex 
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool, ConnectionPool)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Architecture.Simple.Gateway.UserGateway as Gateway
import qualified Driver.UserDb.UserDriver as Driver
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)

handleSaveUserRequest :: ConnectionPool -> UnvalidatedUser -> NotificationSettings -> Handler String
handleSaveUserRequest pool user notificationSettings = do
  liftIO $ flip runSqlPool pool do
    let
      gatewayPort = Gateway.UserGatewayPort {
        saveUser = Driver.saveUser,
        saveNotificationSettings = Driver.saveNotificationSettings
      }
      port = UserUsecasePort {
        saveUser = Gateway.saveUser gatewayPort,
        saveNotificationSettings = Gateway.saveNotificationSettings gatewayPort
      }
    execute port user notificationSettings
    pure "OK"
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]
  

-- もっときれいにできる
logError :: (MonadIO m, Show a) => a -> m ()
logError e = runStdoutLoggingT $ logErrorN $ T.pack $ show e