module Driver.Beam.UserDriver where

import Database.Beam.Postgres (runBeamPostgresDebug)
import Driver.Beam.Config (connection)
import Database.Beam (runInsert, insert)
import Driver.Beam.Database (UserDb(..), userDb)
import Database.Beam.Query (insertValues)
import Driver.Beam.Entity.User (User)
import Driver.Beam.Entity.UserNotification (UserNotification)

updateUser :: User -> IO ()
updateUser user = do
  conn <- connection
  runBeamPostgresDebug putStrLn conn $ do
    runInsert $ insert userDb.users $ insertValues [user]

updateNotificationSettings :: UserNotification -> IO ()
updateNotificationSettings notification = do
  conn <- connection
  runBeamPostgresDebug putStrLn conn $ do
    runInsert $ insert userDb.user_notifications $ insertValues [notification]