{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Gateway.NotificationGateway where

import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem)
import Domain.Message (Message, HasValue(..))
import Control.Lens ((^.))

sendNotification :: Member Port.NotificationGatewayPort r => Message -> Sem r ()
sendNotification message = Port.sendNotification $ message ^. value

