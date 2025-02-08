{-# LANGUAGE FlexibleInstances #-}
module Domain.Email where

import Control.Lens (makeFieldsId)

newtype Email = Email { value :: String }
  deriving (Eq, Show)
makeFieldsId ''Email

newtype UnvalidatedEmail = UnvalidatedEmail { value :: String }
  deriving (Eq, Show)
makeFieldsId ''UnvalidatedEmail