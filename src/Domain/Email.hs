{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Domain.Email
  ( Email(value)
  , HasValue(..)
  , UnvalidatedEmail(..)
  , makeEmail
  , unsafeMakeEmail
  , EmailError(..)
  )
where

import Data.ByteString.Char8 (pack, unpack)
import Text.Email.Validate (validate, EmailAddress, toByteString)
import Control.Lens (makeFieldsId, Bifunctor (bimap))
import Control.Exception (Exception)

newtype Email = Email { value :: String }
  deriving (Eq, Show)
makeFieldsId ''Email

newtype UnvalidatedEmail = UnvalidatedEmail { value :: String }
  deriving (Eq, Show)
makeFieldsId ''UnvalidatedEmail

makeEmail :: String -> Either EmailError Email
makeEmail = bimap InvalidEmailFormat (Email . unpack . toByteString) . validateEmailFormat

unsafeMakeEmail :: String -> Email
unsafeMakeEmail = Email

validateEmailFormat :: String -> Either String EmailAddress
validateEmailFormat email = validate $ pack email

data EmailError = InvalidEmailFormat String
  deriving (Eq, Show)

instance Exception EmailError