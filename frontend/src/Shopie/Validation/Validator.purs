module Shopie.Validation.Validation where

import Prelude

import Data.Either (Either(..), either)
import Data.List (List(Nil), (:))
import Data.Lens (Lens', set)
import Data.Tuple (Tuple(..))
import Data.Monoid (class Monoid, mempty)


type Checker e a b = a -> Either e b
type CheckerM e m a b = a -> m (Either e b)

newtype Validator k v m a b = Validator (a -> m (Either (Tuple k v) b))

newtype Validation e m s a = Validation (s -> m (Tuple a e))

runValidation :: forall e m s a. Validation e m s a -> s -> m (Tuple a e)
runValidation (Validation f) = f

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
    Left e -> pure (Tuple s (e : Nil))
    Right b -> pure (Tuple (set lns b s) Nil)

instance semigroupoidValidator :: Monad m => Semigroupoid (Validator k v m) where
  compose (Validator f) (Validator g) = Validator $ \a ->
    g a >>= either (pure <<< Left) f

instance categoryValidator :: Monad m => Category (Validator k v m) where
  id = Validator $ \x -> pure (Right x)

instance semigroupoidValidation :: (Monad m, Semigroup e) => Semigroupoid (Validation e m) where
  compose (Validation f) (Validation g) = Validation $ \s ->
    g s >>= \(Tuple t e) -> f t >>= \(Tuple u' e') -> pure (Tuple u' (e <> e'))

instance categoryValidation :: (Monad m, Monoid e) => Category (Validation e m) where
  id = Validation $ \s -> pure (Tuple s (mempty :: e))
