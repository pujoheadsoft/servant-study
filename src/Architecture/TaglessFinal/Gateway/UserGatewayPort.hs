module Architecture.TaglessFinal.Gateway.UserGatewayPort where

import Driver.UserDb.Schema (User, UserProfile)

class Monad m => UserGatewayPort m where
  saveUser :: User -> m ()
  saveProfile :: UserProfile -> m ()
