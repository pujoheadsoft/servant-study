module Architecture.Polysemy.Usecase.UserPort where

import Domain.User (User(..), UserProfile, UserId)
import Polysemy (makeSem, Sem, interpret)

data UserPort m a where
  SaveUser :: User -> UserPort m ()
  SaveProfile :: UserId -> UserProfile -> UserPort m ()

makeSem ''UserPort

{-
  ↑の関数だけをControllerで使うやりかたもあるし、↓のようにUserPortFunctionsを作ってrun系関数はこちらで持ってしまうやりかたもある
-}
data UserPortFunctions m = UserPortFunctions {
  saveUser :: User -> m (),
  saveProfile :: UserId -> UserProfile -> m ()
}

runUserPort :: UserPortFunctions (Sem r) -> Sem (UserPort : r) a -> Sem r a
runUserPort f = interpret \case
  SaveUser user -> f.saveUser user
  SaveProfile userId notification -> f.saveProfile userId notification