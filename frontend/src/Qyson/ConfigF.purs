module Qyson.ConfigF
  ( ConfigF
  , configF
  , evalReader
  , evalState
  ) where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.State.Class (class MonadState, gets)

import Data.Profunctor (class Profunctor, dimap, rmap)


data ConfigF c a = ConfigF (c -> a)

configF :: forall c a. (c -> a) -> ConfigF c a
configF = ConfigF

instance profunctorConfigF :: Profunctor ConfigF where
  dimap f g (ConfigF k) = ConfigF (dimap f g k)

instance functorConfigF :: Functor (ConfigF c) where
  map = rmap

evalReader :: forall c m. MonadReader c m => ConfigF c ~> m
evalReader (ConfigF k) = k <$> ask

evalState :: forall c m. MonadState c m => ConfigF c ~> m
evalState (ConfigF k) = gets k
