module Domain.Email where

newtype Email = Email String
  deriving (Eq, Show)

newtype UnvalidatedEmail = UnvalidatedEmail String
  deriving (Eq, Show)