module Architecture.Simple.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..))
import Control.Lens ((^.))
import Control.Exception.Safe (MonadThrow, Exception)
import Control.Exception (throw)
import Control.Arrow ((|||))

eitherThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = throw ||| pure

execute :: (MonadThrow m) => UserUsecasePort m -> UnvalidatedUser -> NotificationSettings -> m ()
execute port user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  port.saveUser u
  port.saveNotificationSettings u.userId notificationSettings

data UserUsecasePort m = UserUsecasePort {
  saveUser :: User -> m (),
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()
}