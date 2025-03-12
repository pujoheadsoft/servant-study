module Architecture.Simple.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), UserProfile (..))

saveUser :: UserGatewayPort m -> User -> m ()
saveUser port (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) first last (pack email.value)
  port.saveUser u

saveProfile :: UserGatewayPort m -> UserId -> UserProfile -> m ()
saveProfile port (UserId userId) (UserProfile bio age githubId) = do
  let p = S.UserProfile (fromIntegral userId) bio (fromIntegral age) githubId
  port.saveProfile p

data UserGatewayPort m = UserGatewayPort {
  saveUser :: S.User -> m (),
  saveProfile :: S.UserProfile -> m ()
}