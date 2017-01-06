module Test.Validation where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Tuple (Tuple)

import Shopie.User.Model as UM
import Shopie.Validation.Validation (Checker, Validation, attach, validation, validation')
import Data.Monoid (class Monoid, mempty)

import Text.Email.Validate as EV

type Account = UM.User UM.UserAttributes

nes :: forall a. (Monoid a, Eq a) => Checker String a a
nes s
  | s == mempty = Left "empty structure"
  | otherwise = Right s


userV :: forall m. Monad m => UM.UserR -> Validation (List (Tuple String String)) m Account Account
userV { firstName, lastName, username, email } =
  validation (UM._attr <<< UM._userR <<< UM._firstName) firstName (attach nes "firstName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._lastName) lastName (attach nes "lastName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._username) username (attach nes "userName")
  >>>
  validation (UM._attr <<< UM._userR <<< UM._email) email (attach (map show <<< EV.validate) "email")

userV' :: forall m. Monad m => Validation (List (Tuple String String)) m Account Account
userV' =
  validation' (UM._attr <<< UM._userR <<< UM._firstName) (attach nes "firstName")
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._lastName) (attach nes "lastName")
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._username) (attach nes "userName")
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._email) (attach (map show <<< EV.validate) "email")
