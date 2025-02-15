{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Architecture.Polysemy.Usecase.SaveUser where

import Domain.User (User(..), UserData (..), UnvalidatedUser(..), NotificationSettings, HasUserData(..), HasUserId(..), HasEmail(..), HasName(..), UserId)
import Domain.Email (makeEmail, HasValue(..), EmailError)
import Control.Lens ((^.))
import Control.Arrow ((|||))
import Polysemy (makeSem, Member, Sem)
import Polysemy.Error (Error, throw)

data UserUsecasePort m a where
  SaveUser :: User -> UserUsecasePort m ()
  SaveNotificationSettings :: UserId -> NotificationSettings -> UserUsecasePort m ()

makeSem ''UserUsecasePort

execute :: (Member UserUsecasePort r, Member (Error EmailError) r) => UnvalidatedUser -> NotificationSettings -> Sem r ()
execute user notificationSettings = do
  validEmail <- eitherThrow . makeEmail $ user ^. userData . email . value
  let
    userName = user ^. userData . name
    u = User (user ^. userId) (UserData userName validEmail)

  saveUser u
  saveNotificationSettings u.userId notificationSettings

eitherThrow :: (Member (Error e) m) => Either e a -> Sem m a
eitherThrow = throw ||| pure
