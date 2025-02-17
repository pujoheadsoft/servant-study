{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Architecture.TaglessFinal.Gateway.NotificationGateway where

import qualified Architecture.TaglessFinal.Gateway.NotificationGatewayPort as Port
import Domain.Message (Message, HasValue(..))
import Control.Lens

sendNotification :: Port.NotificationGatewayPort m => Message -> m ()
sendNotification message = Port.sendNotification $ message ^. value