module Test.Validation where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Tuple (Tuple)

import Shopie.User.Model as UM
import Shopie.Validation.Validation (Checker, attach, validation, Validation)
import Data.Monoid (class Monoid, mempty)

import Text.Email.Validate as EV

type Account = UM.User UM.UserAttributes

nes :: forall a. (Monoid a, Eq a) => Checker String a a
nes s
  | s == mempty = Left "empty structure"
  | otherwise = Right s


userV :: forall m. Monad m => String -> String -> String -> String -> Validation (List (Tuple String String)) m Account Account
userV fn ln u email =
  validation (UM._attr <<< UM._userR <<< UM._firstName) fn (attach nes "firstName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._lastName) ln (attach nes "lastName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._username) u (attach nes "userName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._email) email (attach (map show <<< EV.validate) "email")
