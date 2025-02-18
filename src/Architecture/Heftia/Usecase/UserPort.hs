module Architecture.Heftia.Usecase.UserPort where
import Domain.User
import Control.Monad.Hefty (makeEffectF)

data UserPort a where
  SaveUser :: User -> UserPort ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserPort ()

makeEffectF [''UserPort]