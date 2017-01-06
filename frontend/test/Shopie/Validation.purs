module Test.Shopie.Validation where

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
  validation (UM._attr <<< UM._userR <<< UM._firstName) firstName ("firstName" `attach` nes)
  >>>
  validation (UM._attr <<< UM._userR <<< UM._lastName) lastName ("lastName" `attach` nes)
  >>>
  validation (UM._attr <<< UM._userR <<< UM._username) username ("userName" `attach` nes)
  >>>
  validation (UM._attr <<< UM._userR <<< UM._email) email ("email" `attach` (map show <<< EV.validate))

userV' :: forall m. Monad m => Validation (List (Tuple String String)) m Account Account
userV' =
  validation' (UM._attr <<< UM._userR <<< UM._firstName) ("firstName" `attach` nes)
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._lastName) ("lastName" `attach` nes)
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._username) ("userName" `attach` nes)
  >>>
  validation' (UM._attr <<< UM._userR <<< UM._email) ("email" `attach` (map show <<< EV.validate))
