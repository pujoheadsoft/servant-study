module Architecture.TaglessFinal.Gateway.UserGatewayPort where

import Driver.UserDb.Schema (User, UserNotification)

class Monad m => UserGatewayPort m where
  saveUser :: User -> m ()
  saveNotificationSettings :: UserNotification -> m ()
