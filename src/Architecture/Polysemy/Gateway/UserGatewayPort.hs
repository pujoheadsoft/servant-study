{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Architecture.Polysemy.Gateway.UserGatewayPort where
import Driver.UserDb.Schema (User, UserNotification)
import Polysemy (makeSem, Sem, interpret)

data UserGatewayPort m a where
  SaveUser :: User -> UserGatewayPort m ()
  SaveNotificationSettings :: UserNotification -> UserGatewayPort m ()

makeSem ''UserGatewayPort

data UserGatewayPortFunctions m = UserGatewayPortFunctions {
  saveUser :: User -> m (),
  saveNotificationSettings :: UserNotification -> m ()
}

runUserGatewayPort :: UserGatewayPortFunctions (Sem r) -> Sem (UserGatewayPort : r) a -> Sem r a
runUserGatewayPort f = interpret \case
  SaveUser user -> f.saveUser user
  SaveNotificationSettings notification -> f.saveNotificationSettings notification