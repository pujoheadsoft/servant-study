{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Architecture.Polysemy.Usecase.NotificationPort where

import Polysemy (makeSem, Sem, interpret)
import Domain.Message (Message)

data NotificationPort m a where
  SendNotification :: Message -> NotificationPort m ()

makeSem ''NotificationPort

data NotificationPortFunctions m = UserPortFunctions {
  sendNotification :: Message -> m ()
}

runNotificationPort :: NotificationPortFunctions (Sem r) -> Sem (NotificationPort : r) a -> Sem r a
runNotificationPort f = interpret \case
  SendNotification message -> f.sendNotification message
