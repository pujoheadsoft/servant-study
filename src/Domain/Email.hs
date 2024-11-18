module Domain.Email where

newtype Email = Email String
  deriving (Eq, Show)

newtype UnverifiedEmail = UnverifiedEmail String
  deriving (Eq, Show)