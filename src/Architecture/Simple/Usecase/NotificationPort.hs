module Architecture.Simple.Usecase.NotificationPort where
import Domain.Message (Message)

data NotificationPort m = NotificationPort {
  sendNotification :: Message -> m ()
}