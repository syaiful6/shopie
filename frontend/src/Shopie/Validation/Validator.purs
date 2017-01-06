module Shopie.Validation.Validation where

import Prelude

import Data.Either (Either(..), either)
import Data.List (List(Nil), (:))
import Data.Lens (Lens', set, view)
import Data.Tuple (Tuple(..))
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor (class Profunctor)


type Checker e a b = a -> Either e b
type CheckerM e m a b = a -> m (Either e b)

newtype Validator k v m a b = Validator (a -> m (Either (Tuple k v) b))

newtype Validation e m s a = Validation (s -> m (Tuple e a))

runV :: forall e m s a. Validation e m s a -> s -> m (Tuple e a)
runV (Validation f) = f

attach :: forall k v m a b. Applicative m => Checker v a b -> k -> Validator k v m a b
attach checker field = Validator $ \x ->
  either (pure <<< Left <<< Tuple field) (pure <<< Right) (checker x)

attachM :: forall k v m a b. Monad m => CheckerM v m a b -> k -> Validator k v m a b
attachM checker field = Validator $ \x ->
  checker x >>= either (pure <<< Left <<< Tuple field) (pure <<< Right)

validation
  :: forall k v m a b s
   . Monad m
  => Lens' s b
  -> a
  -> Validator k v m a b
  -> Validation (List (Tuple k v)) m s s
validation lns a (Validator f) = Validation $ \s ->
  f a >>= case _ of
    Left e -> pure (Tuple (e : Nil) s)
    Right b -> pure (Tuple Nil (set lns b s))

-- | a version of validation which input is taken by focusing the field on the data
-- | structure.
validation'
  :: forall k v m a s
   . Monad m
  => Lens' s a
  -> Validator k v m a a
  -> Validation (List (Tuple k v)) m s s
validation' lns (Validator f) = Validation $ \s ->
  f (view lns s) >>= case _ of
    Left e -> pure (Tuple (e : Nil) s)
    Right b -> pure (Tuple Nil (set lns b s))

instance semigroupoidValidator :: Monad m => Semigroupoid (Validator k v m) where
  compose (Validator f) (Validator g) = Validator $ \a ->
    g a >>= either (pure <<< Left) f

instance categoryValidator :: Monad m => Category (Validator k v m) where
  id = Validator $ \x -> pure (Right x)

instance functorValidator :: Functor m => Functor (Validator k v m a) where
  map f (Validator g) = Validator (map (map f) <<< g)

instance applyValidator :: Apply m => Apply (Validator k v m a) where
  apply (Validator f) (Validator g) = Validator \x -> apply <$> f x <*> g x

instance applicativeValidator :: Applicative m => Applicative (Validator k v m a) where
  pure a = Validator (\_ -> pure (Right a))

instance profunctorValidator :: Functor m => Profunctor (Validator k v m) where
  dimap f g (Validator ft) = Validator (f >>> ft >>> map (map g))

instance semigroupoidValidation :: (Monad m, Semigroup e) => Semigroupoid (Validation e m) where
  compose (Validation f) (Validation g) = Validation $ \s ->
    g s >>= \(Tuple e t) -> f t >>= \(Tuple e' u') -> pure (Tuple (e <> e') u')

instance categoryValidation :: (Monad m, Monoid e) => Category (Validation e m) where
  id = Validation $ \s -> pure (Tuple (mempty :: e) s)

instance functorValidation :: Functor m => Functor (Validation e m s) where
  map f (Validation g) = Validation (map (map f) <<< g)

instance applyValidation :: (Apply m, Semigroup e) => Apply (Validation e m s) where
  apply (Validation f) (Validation g) = Validation \x -> apply <$> f x <*> g x

instance applicativeValidation :: (Applicative m, Monoid e) => Applicative (Validation e m s) where
  pure a = Validation (\_ -> pure (pure a))

instance profunctorValidation :: Functor m => Profunctor (Validation e m) where
  dimap f g (Validation ft) = Validation (f >>> ft >>> map (map g))
