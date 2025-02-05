{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Gateway.Simple.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(..))
import Database.Beam.Postgres (runBeamPostgresDebug)
import Driver.Beam.Config (connection)
import Database.Beam (runInsert, insert)
import Driver.Beam.Database (users, userDb)
import Database.Beam.Query (insertValues)
import qualified Driver.Beam.Entity.User as E
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName))

update :: User -> IO ()
update (User (UserId userId) (UserData (UserName first last) (Email email))) = do
  conn <- connection
  runBeamPostgresDebug putStrLn conn $ do
    let u = E.User (fromIntegral userId) (pack first) (pack last) (pack email)
    runInsert $ insert (users userDb) $ insertValues [u]