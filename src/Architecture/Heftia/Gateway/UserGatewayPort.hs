module Architecture.Heftia.Gateway.UserGatewayPort where

import Driver.UserDb.Schema (User, UserProfile)
import Control.Monad.Hefty (makeEffectF)

data UserGatewayPort a where
  SaveUser :: User -> UserGatewayPort ()
  SaveProfile :: UserProfile -> UserGatewayPort ()

makeEffectF [''UserGatewayPort]