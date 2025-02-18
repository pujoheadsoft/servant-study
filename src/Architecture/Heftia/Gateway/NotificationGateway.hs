{-# LANGUAGE FlexibleContexts #-}
module Architecture.Heftia.Gateway.NotificationGateway where

import qualified Architecture.Heftia.Gateway.NotificationGatewayPort as Port
import Prelude hiding (last)

import Domain.Message (Message, HasValue(..))
import Control.Lens ((^.))
import Control.Monad.Hefty (type (<:))

sendNotification :: Port.NotificationGatewayPort <: r => Message -> r ()
sendNotification message = Port.sendNotification $ message ^. value
