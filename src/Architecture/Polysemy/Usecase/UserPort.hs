module Architecture.Polysemy.Usecase.UserPort where

import Domain.User (User(..), NotificationSettings, UserId)
import Polysemy (makeSem, Sem, interpret)

data UserPort m a where
  SaveUser :: User -> UserPort m ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserPort m ()

makeSem ''UserPort

{-
  ↑の関数だけをControllerで使うやりかたもあるし、↓のようにUserPortFunctionsを作ってrun系関数はこちらで持ってしまうやりかたもある
-}
data UserPortFunctions m = UserPortFunctions {
  saveUser :: User -> m (),
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()
}

runUserPort :: UserPortFunctions (Sem r) -> Sem (UserPort : r) a -> Sem r a
runUserPort f = interpret \case
  SaveUser user -> f.saveUser user
  SaveNotificationSettings userId notification -> f.saveNotificationSettings userId notification