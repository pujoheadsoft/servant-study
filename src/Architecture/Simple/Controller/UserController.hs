module Architecture.Simple.Controller.UserController where

import Domain.User (UnvalidatedUser, UserProfile)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Architecture.Simple.Usecase.SaveUser (execute)
import Architecture.Simple.Usecase.UserPort (UserPort(..))
import Architecture.Simple.Usecase.NotificationPort (NotificationPort(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex 
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.Postgresql (runSqlPool, ConnectionPool)
import Control.Monad.IO.Class (liftIO)
import qualified Architecture.Simple.Gateway.UserGateway as UserGateway
import qualified Architecture.Simple.Gateway.NotificationGateway as NotificationGateway
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverWreq as NotificationApiDriverWreq
import Api.Configuration (NotificationApiSettings)
import Common.Logger (logError)

handleSaveUserRequest
  :: NotificationApiSettings
  -> ConnectionPool
  -> UnvalidatedUser
  -> UserProfile
  -> Bool
  -> Handler String
handleSaveUserRequest apiSetting pool user profile withNotify = do
  liftIO $ flip runSqlPool pool do
    let
      userGatewayPort = UserGateway.UserGatewayPort {
        saveUser = UserDriver.saveUser,
        saveProfile = UserDriver.saveProfile
      }
      notificationGateway = NotificationGateway.NotificationGatewayPort {
        sendNotification = NotificationApiDriverWreq.postMessage apiSetting
      }
      userPort = UserPort {
        saveUser = UserGateway.saveUser userGatewayPort,
        saveProfile = UserGateway.saveProfile userGatewayPort
      }
      notificationPort = NotificationPort {
        sendNotification = NotificationGateway.sendNotification notificationGateway
      }
    execute userPort notificationPort user profile withNotify
    pure "OK"
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]
