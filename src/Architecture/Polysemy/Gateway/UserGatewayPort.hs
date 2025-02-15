{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Architecture.Polysemy.Gateway.UserGatewayPort where
import Driver.UserDb.Schema (User, UserNotification)
import Polysemy (makeSem)

data UserGatewayPort m a where
  SaveUser :: User -> UserGatewayPort m ()
  SaveNotificationSettings :: UserNotification -> UserGatewayPort m ()

makeSem ''UserGatewayPort