module Control.Comonad.Cofree.Class where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Env.Trans (EnvT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Control.Comonad.Cofree (Cofree, tail)

import Data.Tuple (Tuple(..))
import Data.Monoid (class Monoid)


class Comonad w <= ComonadCofree f w | w -> f where
  peel :: forall a. w a -> f (w a)

instance comonadCofreeEnvT :: (Functor f, ComonadCofree f w) => ComonadCofree f (EnvT e w) where
  peel (EnvT (Tuple e x)) = EnvT <<< Tuple e <$> peel x

instance comonadCofreeStoreT :: (Functor f, ComonadCofree f w) => ComonadCofree f (StoreT s w) where
  peel (StoreT (Tuple wsa s)) = StoreT <<< (flip Tuple s) <$> peel wsa

instance comonadCofreeTracedT :: (Functor f, Monoid m, ComonadCofree f w) => ComonadCofree f (TracedT m w) where
  peel (TracedT wma) = TracedT <$> peel wma

instance comonadCofreeCofree :: Functor f => ComonadCofree f (Cofree f) where
  peel = tail
