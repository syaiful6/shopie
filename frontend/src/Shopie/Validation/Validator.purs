module Shopie.Validation.Validation
  ( Checker
  , CheckerM
  , Validator(..)
  , Validation(..)
  , censorV
  , attach
  , attachM
  , validation
  , validation'
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)

import Data.Either (Either(..), either)
import Data.List (List(Nil), (:))
import Data.Lens (Lens', set, view)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))


type Checker e a b = a -> Either e b
type CheckerM e m a b = a -> m (Either e b)

newtype Validator k v m a b = Validator (a -> m (Either (Tuple k v) b))

newtype Validation e m s a = Validation (s -> m (Tuple (Maybe e) a))

censorV
  :: forall e m s a
   . Monad m
  => Validation e m s a
  -> s
  -> m (Either e a)
censorV (Validation f) = f >=> un
  where
    un (Tuple Nothing a') = pure (Right a')
    un (Tuple (Just e) _) = pure (Left e)

hushV
  :: forall e m s a
   . Monad m
  => Validation e m s a
  -> s
  -> m (Maybe a)
hushV v = map (either (const Nothing) Just) <<< censorV v

attach :: forall k v m a b. Applicative m => k -> Checker v a b -> Validator k v m a b
attach field checker = Validator $ \x ->
  either (pure <<< Left <<< Tuple field) (pure <<< Right) (checker x)

attachM :: forall k v m a b. Monad m => k -> CheckerM v m a b -> Validator k v m a b
attachM field checker = Validator $ \x ->
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
    Left e -> pure (Tuple (Just (e : Nil)) s)
    Right b -> pure (Tuple Nothing (set lns b s))

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
    Left e -> pure (Tuple (Just (e : Nil)) s)
    Right b -> pure (Tuple Nothing (set lns b s))

derive instance newtypeValidator :: Newtype (Validator k v m a b) _

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

instance altValidator :: (Semigroup k, Semigroup v, Monad m) => Alt (Validator k v m a) where
  alt (Validator f) (Validator g) = Validator \a -> do
    rm <- f a
    case rm of
      Right x -> pure (Right x)
      Left err -> do
        rn <- g a
        case rn of
          Right x' -> pure (Right x')
          Left err' -> pure (Left (err <> err'))

instance plusValidator :: (Monoid k, Monoid v, Monad m) => Plus (Validator k v m a) where
  empty = Validator \_ -> pure (Left mempty)

instance alternativeValidator :: (Monoid k, Monoid v, Monad m) => Alternative (Validator k v m a)

instance profunctorValidator :: Functor m => Profunctor (Validator k v m) where
  dimap f g (Validator ft) = Validator (f >>> ft >>> map (map g))

instance strongValidator :: Functor m => Strong (Validator k v m) where
  first  (Validator f) = Validator \(Tuple s x) -> map (map (_ `Tuple` x)) (f s)
  second (Validator f) = Validator \(Tuple x s) -> map (map (Tuple x)) (f s)

instance choiceValidator :: Applicative m => Choice (Validator k v m) where
  left  (Validator f) = Validator $ either (map (map Left) <<< f) (pure <<< Right <<< Right)
  right (Validator f) = Validator $ either (pure <<< Right <<< Left) (map (map Right) <<< f)

-- Validation

derive instance newtypeValidation :: Newtype (Validation e m s a) _

instance semigroupoidValidation :: (Monad m, Semigroup e) => Semigroupoid (Validation e m) where
  compose (Validation f) (Validation g) = Validation $ \s ->
    g s >>= \(Tuple e t) -> f t >>= \(Tuple e' u') -> pure (Tuple (e <> e') u')

instance categoryValidation :: (Monad m, Semigroup e) => Category (Validation e m) where
  id = Validation $ \s -> pure (Tuple Nothing s)

instance functorValidation :: Functor m => Functor (Validation e m s) where
  map f (Validation g) = Validation (map (map f) <<< g)

instance applyValidation :: (Apply m, Semigroup e) => Apply (Validation e m s) where
  apply (Validation f) (Validation g) = Validation \x -> apply <$> f x <*> g x

instance applicativeValidation :: (Applicative m, Semigroup e) => Applicative (Validation e m s) where
  pure a = Validation (\_ -> pure (pure a))

instance profunctorValidation :: Functor m => Profunctor (Validation e m) where
  dimap f g (Validation ft) = Validation (f >>> ft >>> map (map g))

instance strongValidation :: Functor m => Strong (Validation e m) where
  first  (Validation f) = Validation \(Tuple s x) -> map (map (_ `Tuple` x)) (f s)
  second (Validation f) = Validation \(Tuple x s) -> map (map (Tuple x)) (f s)

instance choiceValidation :: Applicative m => Choice (Validation e m) where
  left  (Validation f) =
    Validation $ either (map (map Left) <<< f) (pure <<< Tuple Nothing <<< Right)
  right (Validation f) =
    Validation $ either (pure <<< Tuple Nothing <<< Left) (map (map Right) <<< f)
