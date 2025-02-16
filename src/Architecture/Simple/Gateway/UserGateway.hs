module Architecture.Simple.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), NotificationSettings (..))
import Control.Exception.Safe (MonadThrow)

saveUser :: (MonadThrow m) => UserGatewayPort m -> User -> m ()
saveUser port (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) first last (pack email.value)
  port.saveUser u

saveNotificationSettings :: (MonadThrow m) => UserGatewayPort m -> UserId -> NotificationSettings -> m ()
saveNotificationSettings port (UserId userId) (NotificationSettings email push) = do
  let n = S.UserNotification (fromIntegral userId) email push
  port.saveNotificationSettings n

data UserGatewayPort m = UserGatewayPort {
  saveUser :: S.User -> m (),
  saveNotificationSettings :: S.UserNotification -> m ()
}