module Control.Monad.CoT where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Class (class ComonadAsk, class ComonadEnv, ask, local)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Comonad.Store.Class (class ComonadStore, peek, pos)
import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Functor.Pairing (Pairing)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

newtype CoT w m a = CoT (forall r. w (a -> m r) -> m r)

runCoT :: forall w m a r. CoT w m a -> w (a -> m r) -> m r
runCoT (CoT cot) = cot

type Co w = CoT w Identity

co :: forall w a. Functor w => (forall r. w (a -> r) -> r) -> Co w a
co f = CoT (Identity <<< f <<< map (map unwrap))

runCo :: forall w a r. Functor w => Co w a -> w (a -> r) -> r
runCo m = unwrap <<< runCoT m <<< map (map Identity)

lowerCoT :: forall w m a s. (Functor w, Monad m) => CoT w m s -> w a -> m s
lowerCoT m = runCoT m <<< (pure <$ _)

liftCoT :: forall w m s. Comonad w => (forall a. w a -> s) -> CoT w m s
liftCoT f = CoT (extract <*> f)

pairCo :: forall w. Functor w => Pairing w (CoT w Identity)
pairCo f w cow = runCo cow (map f w)

instance functorCoT :: Functor w => Functor (CoT w m) where
  map f (CoT cot) = CoT \w -> cot (map (_ <<< f) w)

instance applyCoT :: Extend w => Apply (CoT w m) where
  apply (CoT f) (CoT a) = CoT \w -> f (w =>> \wf g -> a (map (_ <<< g) wf))

instance applicativeCoT :: Comonad w => Applicative (CoT w m) where
  pure a = CoT \w -> extract w a

instance bindCoT :: Extend w => Bind (CoT w m) where
  bind (CoT k) f = CoT \w -> k (w =>> \wa a -> runCoT (f a) wa)

instance monadCoT :: Comonad w => Monad (CoT w m)

instance monadAskCoT :: ComonadAsk e w => MonadAsk e (CoT w m) where
  ask = liftCoT (ask :: forall a. w a -> e)

instance monadReaderCoT :: ComonadEnv e w => MonadReader e (CoT w m) where
  local f (CoT x) = CoT (x <<< local f)

instance monadStateCoT :: ComonadStore s w => MonadState s (CoT w m) where
  state f = do
    s <- liftCoT pos
    case f s of
      Tuple a s1 -> CoT \w -> peek s1 w a

instance monadTellCoT :: ComonadTraced t w => MonadTell t (CoT w m) where
  tell t = CoT \w -> track t w unit

instance monadTransCoT :: Comonad w => MonadTrans (CoT w) where
  lift m = CoT (extract <<< map (m >>= _))

instance monadEffCoT :: (Comonad w, MonadEff eff m) => MonadEff eff (CoT w m) where
  liftEff = lift <<< liftEff

instance monadAffCoT :: (Comonad w, Affable eff m, Monad m) => Affable eff (CoT w m) where
  fromAff = lift <<< fromAff
