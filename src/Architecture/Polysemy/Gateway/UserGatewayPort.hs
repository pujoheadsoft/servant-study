{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Architecture.Polysemy.Gateway.UserGatewayPort where
import Driver.UserDb.Schema (User, UserProfile)
import Polysemy (makeSem, Sem, interpret)

data UserGatewayPort m a where
  SaveUser :: User -> UserGatewayPort m ()
  SaveProfile :: UserProfile -> UserGatewayPort m ()

makeSem ''UserGatewayPort

data UserGatewayPortFunctions m = UserGatewayPortFunctions {
  saveUser :: User -> m (),
  saveProfile :: UserProfile -> m ()
}

runUserGatewayPort :: UserGatewayPortFunctions (Sem r) -> Sem (UserGatewayPort : r) a -> Sem r a
runUserGatewayPort f = interpret \case
  SaveUser user -> f.saveUser user
  SaveProfile notification -> f.saveProfile notification