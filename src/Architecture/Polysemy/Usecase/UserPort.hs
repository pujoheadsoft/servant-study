module Architecture.Polysemy.Usecase.UserPort where

import Domain.User (User(..), NotificationSettings, UserId)
import Polysemy (makeSem)

data UserPort m a where
  SaveUser :: User -> UserPort m ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserPort m ()

makeSem ''UserPort
