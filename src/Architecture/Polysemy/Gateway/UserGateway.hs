{-# LANGUAGE FlexibleContexts #-}
module Architecture.Polysemy.Gateway.UserGateway where

import Data.Text (pack)
import Domain.Email (Email(value))
import qualified Driver.UserDb.Schema as S
import Domain.User (User(..), UserId(..), UserData(UserData), UserName (UserName), NotificationSettings (..))
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as Port
import Prelude hiding (last)
import Polysemy (Member, Sem)

saveUser :: (Member Port.UserGatewayPort r) => User -> Sem r ()
saveUser (User (UserId userId) (UserData (UserName first last) email)) = do
  let u = S.User (fromIntegral userId) (pack first) (pack last) (pack email.value)
  Port.saveUser u

saveNotificationSettings :: (Member Port.UserGatewayPort r) => UserId -> NotificationSettings -> Sem r ()
saveNotificationSettings (UserId userId) (NotificationSettings email push) = do
  let n = S.UserNotification (fromIntegral userId) email push
  Port.saveNotificationSettings n
