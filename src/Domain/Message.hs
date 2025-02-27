{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}
module Domain.Message (Message, registeredUserMessage, HasValue(..)) where

import Control.Lens (makeFieldsId)
import Data.Text (Text)
import Data.String.Interpolate (i)
import Domain.User

newtype Message = Message { value :: Text }
  deriving newtype (Eq, Show)
makeFieldsId ''Message

registeredUserMessage :: UserId -> UserName -> Message
registeredUserMessage userId userName = do
  let n = userName.last <> userName.first
  Message [i|ユーザーID: #{userId}で、#{n}さんが登録されました。|]
