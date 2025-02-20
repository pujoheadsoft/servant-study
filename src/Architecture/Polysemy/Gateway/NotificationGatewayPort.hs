{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Architecture.Polysemy.Gateway.NotificationGatewayPort where

import Data.Text (Text)
import Polysemy (makeSem, Sem, interpret)

data NotificationGatewayPort m a where
  SendNotification :: Text -> NotificationGatewayPort m ()

makeSem ''NotificationGatewayPort

data NotificationGatewayPortFunctions m = NotificationGatewayPortFunctions {
  sendNotification :: Text -> m ()
}

runNotificationGatewayPort
  :: NotificationGatewayPortFunctions (Sem r)
  -> Sem (NotificationGatewayPort : r) a -> Sem r a
runNotificationGatewayPort f = interpret \case
  SendNotification message -> f.sendNotification message