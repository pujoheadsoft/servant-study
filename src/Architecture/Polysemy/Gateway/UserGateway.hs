{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), NotificationSettings (..))
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem, interpret, Embed, embed)
import qualified Architecture.Polysemy.Usecase.UserPort as UserPort

saveUser :: (Member Port.UserGatewayPort r) => User -> Sem r ()
saveUser (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) first last (pack email.value)
  Port.saveUser u

saveNotificationSettings :: (Member Port.UserGatewayPort r) => UserId -> NotificationSettings -> Sem r ()
saveNotificationSettings (UserId userId) (NotificationSettings email push) = do
  let n = S.UserNotification (fromIntegral userId) email push
  Port.saveNotificationSettings n


{-
  ↑の関数だけをControllerで使うやりかたもあるし、↓のようにUserPortFunctionsを作ってrun系関数はこちらで持ってしまうやりかたもある
-}
data UserPortFunctions m = UserPortFunctions {
  saveUser :: User -> m (),
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()
}

createUserPortFunctions :: (Member Port.UserGatewayPort r) => UserPortFunctions (Sem r)
createUserPortFunctions = UserPortFunctions {
  saveUser = saveUser,
  saveNotificationSettings = saveNotificationSettings
}

runUserPort :: Member (Embed m) r => UserPortFunctions m -> Sem (UserPort.UserPort : r) a -> Sem r a
runUserPort f = interpret \case
  UserPort.SaveUser user -> embed $ f.saveUser user
  UserPort.SaveNotificationSettings userId notification -> embed $ f.saveNotificationSettings userId notification