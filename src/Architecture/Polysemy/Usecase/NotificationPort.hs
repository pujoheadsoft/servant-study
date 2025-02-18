module Architecture.Polysemy.Usecase.NotificationPort where

import Polysemy (makeSem)
import Domain.Message (Message)

data NotificationPort m a where
  SendNotification :: Message -> NotificationPort m ()

makeSem ''NotificationPort
