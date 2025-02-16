module Architecture.Simple.Usecase.UserPort where
import Domain.User (User, UserId, NotificationSettings)

data UserPort m = UserPort {
  saveUser :: User -> m (),
  saveNotificationSettings :: UserId -> NotificationSettings -> m ()
}