module Architecture.TaglessFinal.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UserProfile, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..))
import Control.Lens ((^.))
import Control.Exception.Safe (MonadThrow, Exception)
import Control.Exception (throw)
import Control.Arrow ((|||))
import Architecture.TaglessFinal.Usecase.UserPort
import Architecture.TaglessFinal.Usecase.NotificationPort
import Control.Monad (when)
import Domain.Message (registeredUserMessage)

execute
  :: (MonadThrow m, UserPort m, NotificationPort m)
  => UnvalidatedUser
  -> UserProfile
  -> Bool
  -> m ()
execute user profile withNotify = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveProfile u.userId profile

  when withNotify do
    sendNotification (registeredUserMessage u.userId userName)

eitherThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = throw ||| pure