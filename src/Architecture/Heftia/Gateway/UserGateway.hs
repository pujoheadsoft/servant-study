module Architecture.Heftia.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), UserProfile (..))
import qualified Architecture.Heftia.Gateway.UserGatewayPort as Port
import Prelude hiding (last)
import Control.Monad.Hefty (type (<:))

saveUser :: Port.UserGatewayPort <: m => User -> m ()
saveUser (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) first last (pack email.value)
  Port.saveUser u

saveProfile :: Port.UserGatewayPort <: m => UserId -> UserProfile -> m ()
saveProfile (UserId userId) (UserProfile bio age githubId) = do
  let p = S.UserProfile (fromIntegral userId) bio (fromIntegral age) githubId
  Port.saveProfile p
