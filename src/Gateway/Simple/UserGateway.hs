{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Gateway.Simple.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(..))
import qualified Driver.Beam.UserDriver as UserDriver
import qualified Driver.Beam.Entity.User as E
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName))

update :: User -> IO ()
update (User (UserId userId) (UserData (UserName first last) (Email email))) = do
  let u = E.User (fromIntegral userId) (pack first) (pack last) (pack email)
  UserDriver.update u