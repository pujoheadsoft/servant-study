module Architecture.Simple.Usecase.UpdateUser where

import Architecture.Simple.Gateway.UserGateway (updateUser, updateNotificationSettings)
import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..))
import Domain.Email (makeEmail, HasValue(..))
import Control.Lens ((^.))
import Control.Exception.Safe (MonadThrow, Exception)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (throw)
import Control.Arrow ((|||))

eitherThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = throw ||| pure

execute :: (MonadThrow m, MonadIO m) => UnvalidatedUser -> NotificationSettings -> m ()
execute user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  updateUser u
  updateNotificationSettings u.userId notificationSettings