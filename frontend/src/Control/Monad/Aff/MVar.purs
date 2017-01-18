module Control.Monad.Aff.MVar
  ( MVar
  , MVAR
  , AffMVar
  , makeMVar
  , makeMVar'
  , takeMVar
  , putMVar
  , killMVar
  ) where

import Prelude

import Control.Monad.Aff (Aff, nonCanceler)
import Control.Monad.Eff.Exception (Error())

import Data.Function.Uncurried (Fn2, Fn3, runFn3, runFn2)

import Unsafe.Coerce (unsafeCoerce)

foreign import data MVar :: * -> *

foreign import data MVBox :: * -> *

foreign import data MVAR :: !

type AffMVar e a = Aff (mvar :: MVAR | e) a

makeMVar :: forall e a. AffMVar e (MVar a)
makeMVar = fromMVBox $ _makeMVar nonCanceler

makeMVar' :: forall e a. a -> AffMVar e (MVar a)
makeMVar' a = makeMVar >>= \v -> putMVar v a *> pure v

takeMVar :: forall e a. MVar a -> AffMVar e a
takeMVar mv = fromMVBox $ runFn2 _takeMVar nonCanceler mv

putMVar :: forall e a. MVar a -> a -> AffMVar e Unit
putMVar mv a = fromMVBox $ runFn3 _putMVar nonCanceler mv a

-- | Kills an mvar.
killMVar :: forall e a. MVar a -> Error -> AffMVar e Unit
killMVar mv e = fromMVBox $ runFn3 _killMVar nonCanceler mv e

fromMVBox :: forall a e. MVBox a -> AffMVar e a
fromMVBox = unsafeCoerce

-- Internal
foreign import _makeMVar :: forall c a. c -> MVBox (MVar a)

foreign import _takeMVar :: forall c a. Fn2 c (MVar a) (MVBox a)

foreign import _putMVar :: forall c a. Fn3 c (MVar a) a (MVBox Unit)

foreign import _killMVar :: forall c a. Fn3 c (MVar a) Error (MVBox Unit)
