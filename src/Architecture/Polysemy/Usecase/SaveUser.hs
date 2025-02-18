module Architecture.Polysemy.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..))
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Architecture.Polysemy.Usecase.UserPort (UserUsecasePort, saveUser, saveNotificationSettings)

execute :: (Member UserUsecasePort r, Member (Error EmailError) r) => UnvalidatedUser -> NotificationSettings -> Sem r ()
execute user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveNotificationSettings u.userId notificationSettings

eitherThrow :: (Member (Error e) m) => Either e a -> Sem m a
eitherThrow = throw ||| pure
