module Data.Qyson.ConfigF where

import Prelude

import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Free (Free, liftF)

import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Tuple (Tuple(Tuple))
import Data.Functor.Pairing (Pairing, sym, fnTuple)

data ConfigF c a = ConfigF (c -> a)

configF :: forall c. ConfigF c c
configF = ConfigF id

ask :: forall c. Free (ConfigF c) c
ask = liftF configF

instance profunctorConfigF :: Profunctor ConfigF where
  dimap f g (ConfigF k) = ConfigF (dimap f g k)

instance functorConfigF :: Functor (ConfigF c) where
  map = rmap

data CoconfigF c a = CoconfigF (Tuple c a)

instance functorCoconfig :: Functor (CoconfigF c) where
  map f (CoconfigF t) = CoconfigF (map f t)

coConfigF :: forall c a. c -> a -> CoconfigF c a
coConfigF c a = CoconfigF $ Tuple c a

configPairing :: forall c. Pairing (CoconfigF c) (ConfigF c)
configPairing f (CoconfigF t) (ConfigF k) = sym fnTuple f t k

mkCoconfig :: forall c a. c -> a -> Cofree (CoconfigF c) a
mkCoconfig c a = unfoldCofree a id (coConfigF c)
