module Architecture.Polysemy.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UserProfile, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..))
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Polysemy (Member, Sem, Members)
import Polysemy.Error (Error, throw)
import Architecture.Polysemy.Usecase.UserPort (UserPort, saveUser, saveProfile)
import Architecture.Polysemy.Usecase.NotificationPort (NotificationPort, sendNotification)
import Control.Monad (when)
import Domain.Message (registeredUserMessage)

execute
  :: (Members '[UserPort, NotificationPort, Error EmailError] r)
  => UnvalidatedUser
  -> UserProfile
  -> Bool
  -> Sem r ()
execute user profile withNotify = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveProfile u.userId profile

  when withNotify do
    sendNotification (registeredUserMessage u.userId userName)

eitherThrow :: (Member (Error e) m) => Either e a -> Sem m a
eitherThrow = throw ||| pure
