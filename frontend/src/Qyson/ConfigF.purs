module Qyson.ConfigF
  ( ConfigF
  , configF
  , evalReader
  , evalState
  , evalAp
  ) where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.State.Class (class MonadState, gets)


data ConfigF c a = ConfigF (c -> a)

configF :: forall c a. (c -> a) -> ConfigF c a
configF = ConfigF

instance functorConfigF :: Functor (ConfigF c) where
  map f (ConfigF g) = ConfigF (f <<< g)

evalReader :: forall c m. MonadReader c m => ConfigF c ~> m
evalReader (ConfigF k) = k <$> ask

evalAp :: forall f c a. Applicative f => ConfigF c a -> c -> f a
evalAp (ConfigF k) c = pure (k c)

evalState :: forall c m. MonadState c m => ConfigF c ~> m
evalState (ConfigF k) = gets k
