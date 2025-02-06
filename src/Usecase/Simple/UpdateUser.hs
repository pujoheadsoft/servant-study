module Usecase.Simple.UpdateUser where

import Gateway.Simple.UserGateway (updateUser, updateNotificationSettings)
import Domain.User (User(..), UserData (..), UnvalidatedUser(..), UnvalidatedUserData(..), NotificationSettings)
import Domain.Email (Email(..), UnvalidatedEmail(..))

execute :: UnvalidatedUser -> NotificationSettings -> IO ()
execute user notificationSettings = do
  let
    (UnvalidatedEmail m) = user.userData.email
    u = User user.userId (UserData user.userData.name (Email m))
  updateUser u
  updateNotificationSettings u.userId notificationSettings