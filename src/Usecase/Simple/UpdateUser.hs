{-# LANGUAGE OverloadedRecordDot #-}
module Usecase.Simple.UpdateUser where

import Data.Text (pack)
import Data.Int (Int32)
import Gateway.Simple.UserGateway (update)
import Domain.User (User(..), UserId (..), UserData (..), UserName (..), UnvalidatedUser)
import Domain.Email (Email(..))
import Prelude hiding (last, id)
import Data.Text.Internal.Read (IParser(P))

execute :: UnvalidatedUser -> IO ()
execute user = do
  let u = User (UserId undefined) (UserData undefined (Email  undefined))
  update u