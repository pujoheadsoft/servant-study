module Architecture.Heftia.Usecase.NotificationPort where

import Domain.Message (Message)
import Control.Monad.Hefty (makeEffectF)

data NotificationPort a where
  SendNotification :: Message -> NotificationPort ()

makeEffectF [''NotificationPort]