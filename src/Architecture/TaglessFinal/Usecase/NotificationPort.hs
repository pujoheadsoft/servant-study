module Architecture.TaglessFinal.Usecase.NotificationPort where
import Domain.Message (Message)

class Monad m => NotificationPort m where
  sendNotification :: Message -> m ()
