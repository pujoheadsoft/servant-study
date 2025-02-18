module Architecture.Heftia.Gateway.NotificationGatewayPort where

import Data.Text (Text)
import Control.Monad.Hefty (makeEffectF)

data NotificationGatewayPort a where
  SendNotification :: Text -> NotificationGatewayPort ()

makeEffectF [''NotificationGatewayPort]