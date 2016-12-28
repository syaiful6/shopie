module Data.Qyson.Utils
  ( coiter
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, unfoldCofree)

coiter :: forall a f. Functor f => (a -> f a) -> a -> Cofree f a
coiter psi a = unfoldCofree a id psi
