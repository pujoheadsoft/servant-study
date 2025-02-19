{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Architecture.Polysemy.Gateway.NotificationGateway where

import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem, Embed, interpret, embed)
import Domain.Message (Message, HasValue(..))
import Control.Lens ((^.))
import qualified Architecture.Polysemy.Usecase.NotificationPort as NotificationPort

sendNotification :: Member Port.NotificationGatewayPort r => Message -> Sem r ()
sendNotification message = Port.sendNotification $ message ^. value


data NotificationPortFunctions m = UserPortFunctions {
  sendNotification :: Message -> m ()
}

createNotificationPortFunctions :: (Member Port.NotificationGatewayPort r) => NotificationPortFunctions (Sem r)
createNotificationPortFunctions = UserPortFunctions {
  sendNotification = sendNotification
}

runNotificationPort :: Member (Embed m) r => NotificationPortFunctions m -> Sem (NotificationPort.NotificationPort : r) a -> Sem r a
runNotificationPort f = interpret \case
  NotificationPort.SendNotification message -> embed $ f.sendNotification message