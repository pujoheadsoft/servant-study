module Usecase.Simple.UpdateUser where

import Gateway.Simple.UserGateway (update)
import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UnvalidatedUserData(..))
import Domain.Email (Email(..), UnvalidatedEmail(..))

execute :: UnvalidatedUser -> IO ()
execute user = do
  let
    (UnvalidatedEmail m) = user.userData.email
    u = User user.userId (UserData user.userData.name (Email m))
  update u