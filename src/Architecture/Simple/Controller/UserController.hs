module Architecture.Simple.Controller.UserController where

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Architecture.Simple.Usecase.SaveUser (execute)
import Architecture.Simple.Usecase.UserPort (UserPort(..))
import Architecture.Simple.Usecase.NotificationPort (NotificationPort(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex 
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool, ConnectionPool)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Architecture.Simple.Gateway.UserGateway as UserGateway
import qualified Architecture.Simple.Gateway.NotificationGateway as NotificationGateway
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationApiDriver
import qualified Driver.Api.NotificationApiDriverWreq as NotificationApiDriverWreq
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Api.Configuration (NotificationApiSettings)

handleSaveUserRequest
  :: NotificationApiSettings
  -> ConnectionPool
  -> UnvalidatedUser
  -> NotificationSettings
  -> Bool
  -> Handler String
handleSaveUserRequest apiSetting pool user notificationSettings withNotify = do
  liftIO $ flip runSqlPool pool do
    let
      userGatewayPort = UserGateway.UserGatewayPort {
        saveUser = UserDriver.saveUser,
        saveNotificationSettings = UserDriver.saveNotificationSettings
      }
      notificationGateway = NotificationGateway.NotificationGatewayPort {
        sendNotification = NotificationApiDriverWreq.postMessage apiSetting
      }
      userPort = UserPort {
        saveUser = UserGateway.saveUser userGatewayPort,
        saveNotificationSettings = UserGateway.saveNotificationSettings userGatewayPort
      }
      notificationPort = NotificationPort {
        sendNotification = NotificationGateway.sendNotification notificationGateway
      }
    execute userPort notificationPort user notificationSettings withNotify
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