module Control.Comonad.CofreeT where

import Prelude

import Control.Alternative (class Alternative, (<|>), empty)
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree.Class (class ComonadCofree)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, extend)
import Control.Monad.Trans.Class (class MonadTrans)

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldrDefault, bifoldlDefault)
import Data.Bitraversable (class Bitraversable, bitraverse)
import Data.Foldable (class Foldable, foldr, foldl, foldMap, foldrDefault, foldlDefault)
import Data.Lazy (Lazy, force, defer)
import Data.Traversable (class Traversable, traverse)


data CofreeF f a b = CofreeF a (Lazy (f b))

coiterT :: forall f w a. (Functor f, Comonad w) => (w a -> f (w a)) -> w a -> CofreeT f w a
coiterT psi = CofreeT <<< extend (\w -> CofreeF (extract w) (defer (\_ -> map (coiterT psi) (psi w))))

-- | Create a value of type `CofreeF f a b` from a label and a
-- | functor-full of "subtrees".
mkCofreeF :: forall f a b. a -> f b -> CofreeF f a b
mkCofreeF a t = CofreeF a (defer \_ -> t)

-- | Extract the head of the base functor
headF :: forall f a b. CofreeF f a b -> a
headF (CofreeF a _) = a

-- | Extract the tails of the base functor
tailF :: forall f a b. CofreeF f a b -> f b
tailF (CofreeF _ t) = force t

_tailF :: forall f a b. CofreeF f a b -> Lazy (f b)
_tailF (CofreeF _ t) = t

_map2 :: forall f a b. Functor f => (a -> b) -> Lazy (f a) -> Lazy (f b)
_map2 = map <<< map

instance functorCofreeF :: Functor f => Functor (CofreeF f a) where
  map f fa = CofreeF (headF fa) (_map2 f (_tailF fa))

instance foldableCofreeF :: Foldable f => Foldable (CofreeF f a) where
  foldMap f = foldMap f <<< tailF
  foldr f a = foldr f a <<< tailF
  foldl f a = foldl f a <<< tailF

instance bifoldableCofreeF :: Foldable f => Bifoldable (CofreeF f) where
  bifoldMap f g c@(CofreeF h t) = f h <> foldMap g (tailF c)
  bifoldr = bifoldrDefault
  bifoldl = bifoldlDefault

instance traversableCofreeF :: Traversable f => Traversable (CofreeF f a) where
  sequence = traverse id
  traverse f c@(CofreeF a _) = mkCofreeF a <$> traverse f (tailF c)

instance bitraversableCofreeF :: Traversable f => Bitraversable (CofreeF f) where
  bisequence = bitraverse id id
  bitraverse f g c@(CofreeF a _) = mkCofreeF <$> f a <*> traverse g (tailF c)

instance biFunctorCofreeF :: Functor f => Bifunctor (CofreeF f) where
  bimap f g (CofreeF a t) = CofreeF (f a) (map g <$> t)

newtype CofreeT f w a = CofreeT (w (CofreeF f a (CofreeT f w a)))

runCofreeT :: forall f w a. CofreeT f w a -> w (CofreeF f a (CofreeT f w a))
runCofreeT (CofreeT a) = a

instance functorCofreeT :: (Functor f, Functor w) => Functor (CofreeT f w) where
  map f = CofreeT <<< map (bimap f (map f)) <<< runCofreeT

instance applyCofreeT :: (Alternative f, Apply w) => Apply (CofreeT f w) where
  apply wf wa = CofreeT $ go <$> runCofreeT wf <*> runCofreeT wa
    where
    go (CofreeF f t) a = case bimap f (map f) a of
      CofreeF b n -> mkCofreeF b (force n <|> (map ((flip apply) wa) (force t)))

instance applicativeCofreeT :: (Alternative f, Applicative w) => Applicative (CofreeT f w) where
  pure = CofreeT <<< pure <<< (flip mkCofreeF empty)

instance bindCofreeT :: (Alternative f, Monad w) => Bind (CofreeT f w) where
  bind (CofreeT fa) f = CofreeT $ do
    CofreeF a m <- fa
    CofreeF b n <- runCofreeT $ f a
    pure $ mkCofreeF b (force n <|> (map (flip bind f) (force m)))

instance monadCofreeT :: (Alternative f, Monad w) => Monad (CofreeT f w)

instance comonadCofreeCofreeT :: (Functor f, Comonad w) => ComonadCofree f (CofreeT f w) where
  peel = tailF <<< extract <<< runCofreeT

instance monadTransCofreeT :: Alternative f => MonadTrans (CofreeT f) where
  lift = CofreeT <<< map (flip mkCofreeF empty)

instance foldableCofreeT :: (Foldable f, Foldable w) => Foldable (CofreeT f w) where
  foldMap f = foldMap (bifoldMap f (foldMap f)) <<< runCofreeT
  foldr = foldrDefault
  foldl = foldlDefault

instance traversableCofreeT :: (Traversable f, Traversable w) => Traversable (CofreeT f w) where
  traverse f = map CofreeT <<< traverse (bitraverse f (traverse f)) <<< runCofreeT
  sequence = traverse id

instance extendCofreeT :: (Functor f, Comonad w) => Extend (CofreeT f w) where
  extend f =
    let
      ext w = CofreeF (f (CofreeT w)) (_map2 (extend f) (_tailF (extract w)))
    in
      CofreeT <<< extend ext <<< runCofreeT

instance comonadCofreeT :: (Functor f, Comonad w) => Comonad (CofreeT f w) where
  extract = headF <<< extract <<< runCofreeT

instance comonadTransCofreeT :: ComonadTrans (CofreeT f) where
  lower = map headF <<< runCofreeT
