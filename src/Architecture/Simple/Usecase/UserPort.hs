module Architecture.Simple.Usecase.UserPort where
import Domain.User (User, UserId, UserProfile)

data UserPort m = UserPort {
  saveUser :: User -> m (),
  saveProfile :: UserId -> UserProfile -> m ()
}