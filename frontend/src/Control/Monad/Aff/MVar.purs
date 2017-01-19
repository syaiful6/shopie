module Control.Monad.Aff.MVar
  ( MVar
  , MVAR
  , AffMVar
  , makeMVar
  , makeMVar'
  , takeMVar
  , tryTakeMVar
  , peekMVar
  , putMVar
  , tryPutMVar
  , killMVar
  , modifyMVar
  , modifyMVar'
  , withMVar
  ) where

import Prelude

import Control.Monad.Aff (Aff, nonCanceler)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (throwError, catchError)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)

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

tryTakeMVar :: forall e a. MVar a -> AffMVar e (Maybe a)
tryTakeMVar v = fromMVBox $ runFn4 _tryTakeMVar nonCanceler v Nothing Just

peekMVar :: forall e a. MVar a -> AffMVar e a
peekMVar q = fromMVBox $ runFn2 _peekMVar nonCanceler q

putMVar :: forall e a. MVar a -> a -> AffMVar e Unit
putMVar mv a = fromMVBox $ runFn3 _putMVar nonCanceler mv a

tryPutMVar :: forall e a. MVar a -> a -> AffMVar e Boolean
tryPutMVar v a = fromMVBox $ runFn3 _tryPutMVar nonCanceler v a

-- | Kills an mvar.
killMVar :: forall e a. MVar a -> Error -> AffMVar e Unit
killMVar mv e = fromMVBox $ runFn3 _killMVar nonCanceler mv e

modifyMVar :: forall e a. (a -> a) -> MVar a -> AffMVar e Unit
modifyMVar f v = takeMVar v >>= (f >>> putMVar v)

modifyMVar' :: forall e a b. MVar a -> (a -> AffMVar e (Tuple a b)) -> AffMVar e b
modifyMVar' v aff = do
  a <- takeMVar v
  Tuple a' b <- aff a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a' *> pure b

withMVar :: forall e a b. MVar a -> (a -> AffMVar e b) -> AffMVar e b
withMVar v aff = do
  a <- takeMVar v
  b <- aff a `catchError` \e -> putMVar v a *> throwError e
  putMVar v a *> pure b

fromMVBox :: forall a e. MVBox a -> AffMVar e a
fromMVBox = unsafeCoerce

-- Internal
foreign import _makeMVar :: forall c a. c -> MVBox (MVar a)

foreign import _takeMVar :: forall c a. Fn2 c (MVar a) (MVBox a)

foreign import _tryTakeMVar :: forall c a. Fn4 c (MVar a) (Maybe a) (a -> Maybe a) (MVBox (Maybe a))

foreign import _peekMVar :: forall c a. Fn2 c (MVar a) (MVBox a)

foreign import _putMVar :: forall c a. Fn3 c (MVar a) a (MVBox Unit)

foreign import _tryPutMVar :: forall c a. Fn3 c (MVar a) a (MVBox Boolean)

foreign import _killMVar :: forall c a. Fn3 c (MVar a) Error (MVBox Unit)
