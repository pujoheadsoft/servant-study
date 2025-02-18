module Architecture.Polysemy.Usecase.UserPort where

import Domain.User (User(..), NotificationSettings, UserId)
import Polysemy (makeSem)

data UserUsecasePort m a where
  SaveUser :: User -> UserUsecasePort m ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserUsecasePort m ()

makeSem ''UserUsecasePort
