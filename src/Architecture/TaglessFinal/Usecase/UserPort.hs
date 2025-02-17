module Architecture.TaglessFinal.Usecase.UserPort where
import Domain.User

class Monad m => UserPort m where
  saveUser :: User -> m ()
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()