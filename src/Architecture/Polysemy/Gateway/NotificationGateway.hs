{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Architecture.Polysemy.Gateway.NotificationGateway where

import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem)
import Domain.Message (Message, HasValue(..))
import Control.Lens ((^.))
import Architecture.Polysemy.Usecase.NotificationPort (NotificationPortFunctions(..))

sendNotification :: Member Port.NotificationGatewayPort r => Message -> Sem r ()
sendNotification message = Port.sendNotification $ message ^. value

createNotificationPortFunctions :: (Member Port.NotificationGatewayPort r) => NotificationPortFunctions (Sem r)
createNotificationPortFunctions = UserPortFunctions {
  sendNotification = sendNotification
}
