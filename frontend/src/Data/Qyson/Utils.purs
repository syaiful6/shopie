module Data.Qyson.Utils
  ( coiter
  , pairArrowTuple
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Data.Functor.Pairing (Pairing)
import Data.Tuple (Tuple, uncurry)

coiter :: forall a f. Functor f => (a -> f a) -> a -> Cofree f a
coiter psi a = unfoldCofree a id psi

pairArrowTuple :: forall a. Pairing ((->) a) (Tuple a)
pairArrowTuple f g = uncurry (f <<< g)
