module Architecture.Heftia.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UserProfile, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Control.Monad.Hefty (type (<:))
import Control.Monad.Hefty.Except (Throw, throw)
import Architecture.Heftia.Usecase.UserPort
import Architecture.Heftia.Usecase.NotificationPort
import Control.Monad (when)
import Domain.Message (registeredUserMessage)

execute
  :: (UserPort <: m, NotificationPort <: m, Throw EmailError <: m, Monad m)
  => UnvalidatedUser -> UserProfile -> Bool -> m ()
execute user profile withNotify = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveProfile u.userId profile

  when withNotify do
    sendNotification (registeredUserMessage u.userId userName)

eitherThrow :: (Throw e <: m, Monad m) => Either e a -> m a
eitherThrow = throw ||| pure
