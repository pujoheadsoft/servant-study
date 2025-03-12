{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), UserProfile (..))
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem)
import Architecture.Polysemy.Usecase.UserPort (UserPortFunctions(..))

saveUser :: (Member Port.UserGatewayPort r) => User -> Sem r ()
saveUser (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) first last (pack email.value)
  Port.saveUser u

saveProfile :: (Member Port.UserGatewayPort r) => UserId -> UserProfile -> Sem r ()
saveProfile (UserId userId)  (UserProfile bio age githubId) = do
  let p = S.UserProfile (fromIntegral userId) bio (fromIntegral age) githubId
  Port.saveProfile p

createUserPortFunctions :: (Member Port.UserGatewayPort r) => UserPortFunctions (Sem r)
createUserPortFunctions = UserPortFunctions {
  saveUser = saveUser,
  saveProfile = saveProfile
}

