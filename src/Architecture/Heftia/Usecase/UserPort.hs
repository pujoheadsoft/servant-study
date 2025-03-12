module Architecture.Heftia.Usecase.UserPort where
import Domain.User
import Control.Monad.Hefty (makeEffectF)

data UserPort a where
  SaveUser :: User -> UserPort ()
  SaveProfile :: UserId -> UserProfile -> UserPort ()
makeEffectF [''UserPort]