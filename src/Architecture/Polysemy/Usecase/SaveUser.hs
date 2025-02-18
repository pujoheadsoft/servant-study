module Architecture.Polysemy.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..))
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Polysemy (Member, Sem, Members)
import Polysemy.Error (Error, throw)
import Architecture.Polysemy.Usecase.UserPort (UserPort, saveUser, saveNotificationSettings)
import Architecture.Polysemy.Usecase.NotificationPort (NotificationPort, sendNotification)
import Control.Monad (when)
import Domain.Message (registeredUserMessage)

execute
  :: (Members '[UserPort, NotificationPort, Error EmailError] r)
  => UnvalidatedUser
  -> NotificationSettings
  -> Bool
  -> Sem r ()
execute user notificationSettings withNotify = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveNotificationSettings u.userId notificationSettings

  when withNotify do
    sendNotification (registeredUserMessage u.userId userName)

eitherThrow :: (Member (Error e) m) => Either e a -> Sem m a
eitherThrow = throw ||| pure
