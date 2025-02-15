module Architecture.Heftia.Gateway.UserGatewayPort where

import Driver.UserDb.Schema (User, UserNotification)
import Control.Monad.Hefty (makeEffectF)

data UserGatewayPort a where
  SaveUser :: User -> UserGatewayPort ()
  SaveNotificationSettings :: UserNotification -> UserGatewayPort ()

makeEffectF [''UserGatewayPort]