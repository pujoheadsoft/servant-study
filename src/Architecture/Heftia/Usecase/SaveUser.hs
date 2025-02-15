module Architecture.Heftia.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Control.Monad.Hefty (makeEffectF, type (<:))
import Control.Monad.Hefty.Except (Throw, throw)

data UserUsecasePort a where
  SaveUser :: User -> UserUsecasePort ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserUsecasePort ()

makeEffectF [''UserUsecasePort]

execute :: (UserUsecasePort <: m, Throw EmailError <: m, Monad m) => UnvalidatedUser -> NotificationSettings -> m ()
execute user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveNotificationSettings u.userId notificationSettings

eitherThrow :: (Throw e <: m, Monad m) => Either e a -> m a
eitherThrow = throw ||| pure
