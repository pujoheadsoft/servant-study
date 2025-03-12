module Architecture.Simple.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UserProfile, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserName (..))
import Domain.Email (makeEmail, HasValue(..))
import Control.Lens ((^.))
import Control.Exception.Safe (MonadThrow, Exception)
import Control.Exception (throw)
import Control.Arrow ((|||))
import Architecture.Simple.Usecase.UserPort (UserPort(..))
import Architecture.Simple.Usecase.NotificationPort (NotificationPort(..))
import Domain.Message (registeredUserMessage)
import Control.Monad (when)

eitherThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = throw ||| pure

execute
  :: (MonadThrow m)
  => UserPort m
  -> NotificationPort m
  -> UnvalidatedUser
  -> UserProfile
  -> Bool
  -> m ()
execute userPort notificationPort user profile withNotify = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  userPort.saveUser u
  userPort.saveProfile u.userId profile

  when withNotify do
    notificationPort.sendNotification (registeredUserMessage u.userId userName)
