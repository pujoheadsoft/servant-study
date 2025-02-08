module Domain.Email where

newtype Email = Email { value :: String }
  deriving (Eq, Show)

newtype UnvalidatedEmail = UnvalidatedEmail { value :: String }
  deriving (Eq, Show)