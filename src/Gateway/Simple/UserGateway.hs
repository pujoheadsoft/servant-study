{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Gateway.Simple.UserGateway where
-- import Domain.User (ValidatedExistingUser (..), ValidatedUserData (..), UserName (..), UserId (..))
import Data.Text (pack)
import Domain.Email (Email(..))
import Database.Beam.Postgres (runBeamPostgresDebug)
import Driver.Beam.Config (connection)
import Database.Beam (runInsert, insert)
import Driver.Beam.Database (users, userDb)
import Database.Beam.Query (insertValues)
import Driver.Beam.Entity.User (UserT (User))

-- update :: ValidatedExistingUser -> IO ()
-- update (ValidatedExistingUser (UserId userId) (ValidatedUserData (UserName first last) (Email email))) = do
--   conn <- connection
--   runBeamPostgresDebug putStrLn conn $ do
--     let u = User (fromIntegral userId) (pack first) (pack last) (pack email)
--     runInsert $ insert (users userDb) $ insertValues [u]