module Usecase.Simple.UpdateUser where

import Database.Beam.Postgres (runBeamPostgresDebug)
import Driver.Beam.Config (connection)
import Database.Beam (runUpdate, save)
import Driver.Beam.Database (userDb, UserDb (users))
import Driver.Beam.Entity.User
import Data.Text (pack)
import Data.Int (Int32)

execute :: IO ()
execute = do
  conn <- connection
  runBeamPostgresDebug putStrLn conn $ do
    let u = User (1 :: Int32) (pack "Galileo") (pack "Galilei") (pack "")
    runUpdate $ save (users userDb) u