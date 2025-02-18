module Architecture.Polysemy.Gateway.NotificationGatewayPort where

import Data.Text (Text)
import Polysemy (makeSem)

data NotificationGatewayPort m a where
  SendNotification :: Text -> NotificationGatewayPort m ()

makeSem ''NotificationGatewayPort