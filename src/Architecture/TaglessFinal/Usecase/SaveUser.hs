module Architecture.TaglessFinal.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..))
import Control.Lens ((^.))
import Control.Exception.Safe (MonadThrow, Exception)
import Control.Exception (throw)
import Control.Arrow ((|||))

execute :: (MonadThrow m, UserUsecasePort m) => UnvalidatedUser -> NotificationSettings -> m ()
execute user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveNotificationSettings u.userId notificationSettings

class Monad m => UserUsecasePort m where
  saveUser :: User -> m ()
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()

eitherThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = throw ||| pure