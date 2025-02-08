module Usecase.Simple.UpdateUser where

import Gateway.Simple.UserGateway (updateUser, updateNotificationSettings)
import Domain.User -- (User(..), UserData (..), UnvalidatedUser(..), UnvalidatedUserData(..), NotificationSettings)
import Domain.Email (Email(..), UnvalidatedEmail(..))
import Control.Lens

execute :: UnvalidatedUser -> NotificationSettings -> IO ()
execute user notificationSettings = do
  let
    d = user.userData.email.value
    u = User user.userId (UserData user.userData.name (Email d))
  updateUser u
  updateNotificationSettings u.userId notificationSettings