module Control.Applicative.Free where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)

import Data.Const (Const(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)

import Data.Functor.Day (Day, day, runDay)


data FreeAp f a
  = Pure a
  | Ap ((Day f (FreeAp f)) a)

runAp :: forall f g. Applicative g => (f ~> g) -> FreeAp f ~> g
runAp _ (Pure a) = pure a
runAp k (Ap d) = runDay (\i f g -> pure i <*> k f <*> runAp k g) d

-- | lift a value to FreeAp
liftAp :: forall f a. f a -> FreeAp f a
liftAp x = Ap $ day const x $ Pure unit

-- | Given a natural transformation from 'f' to g this gives a monoidal
-- | natural transformation from FreeAp f to FreeAp g.
hoistAp :: forall f g. (f ~> g) -> FreeAp f ~> FreeAp g
hoistAp _ (Pure a) = pure a
hoistAp k (Ap d) = runDay (\i f g -> Ap (day i (k f) (hoistAp k g))) d

-- | Interprets the free applicative functor over f using the semantics for
-- | `pure` and `<*>` given by the Applicative instance for f.
restractAp :: forall f a. Applicative f => FreeAp f a -> f a
restractAp (Pure a) = pure a
restractAp (Ap d) = runDay (\i f g -> i <$> f <*> restractAp g) d

-- | Perform monoidal analysis over the free applicative functor `f`.
analyzeFreeAp :: forall f m a. Monoid m => (forall b. f b -> m) -> FreeAp f a -> m
analyzeFreeAp k = unwrap <<< runAp (Const <<< k)

instance functorFreeAp :: Functor (FreeAp f) where
  map k (Pure a) = Pure (k a)
  map k (Ap d) = Ap (k <$> d)

instance applyFreeAp :: Apply (FreeAp f) where
  apply (Pure k) (Pure a) = Pure (k a)
  apply (Pure k) (Ap d) = Ap (k <$> d)
  apply (Ap d) e =
    runDay (\i f g -> Ap (day (#) f (pure (\y a -> (\x -> i x y a)) <*> g <*> e))) d

instance applicativeFreeAp :: Applicative (FreeAp f) where
  pure = Pure

instance extendFreeAp :: Extend f => Extend (FreeAp f) where
  extend k w@(Pure _) = Pure (k w)
  extend k (Ap d) = Ap (extend (k <<< Ap) d)

instance comonadFreeAp :: Comonad f => Comonad (FreeAp f) where
  extract (Pure a) = a
  extract (Ap d) = extract d
