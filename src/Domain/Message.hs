{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Domain.Message (Message, registeredUserMessage, HasValue(..)) where

import Control.Lens (makeFieldsId)
import Data.Text (Text, pack)
import Domain.User

newtype Message = Message { value :: Text }
  deriving newtype (Eq, Show)
makeFieldsId ''Message

registeredUserMessage :: UserId -> UserName -> Message
registeredUserMessage userId userName = do
  let n = userName.last <> userName.first
  Message $ pack "ユーザーID: " <> pack (show userId) <> pack "で、" <> n <> pack "さんが登録されました。"