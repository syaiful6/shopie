module Data.Functor.Pairing
  ( Pairing
  , zap
  , sym
  , identity
  , fnTuple
  , productCoproduct
  , stateStore
  , readerEnv
  , writerTraced
  , freeCofree
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Control.Comonad.Env.Trans (EnvT(..))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Control.Monad.Free (Free, resume)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Data.Either (Either(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), uncurry)


type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

zap :: forall f g a b. Pairing f g -> f (a -> b) -> g a -> b
zap pairing = pairing ($)

-- | Pairing is symmetric
sym :: forall f g. Pairing f g -> Pairing g f
sym pairing f ga fb = pairing (flip f) fb ga

-- | The identity functor pairs with itself
identity :: Pairing Identity Identity
identity f (Identity a) (Identity b) = f a b

-- | Functor products pair with functor coproducts
productCoproduct
  :: forall f1 g1 f2 g2
   . Pairing f1 g1
  -> Pairing f2 g2
  -> Pairing (Product f1 f2) (Coproduct g1 g2)
productCoproduct p1 p2 f (Product (Tuple f1 f2)) (Coproduct e) =
  case e of
    Left g1 -> p1 f f1 g1
    Right g2 -> p2 f f2 g2

fnTuple :: forall a. Pairing ((->) a) (Tuple a)
fnTuple p f = uncurry (p <<< f)

-- | `StateT` pairs with `StoreT`.
stateStore :: forall f g s. Pairing f g -> Pairing (StateT s f) (StoreT s g)
stateStore pairing f (StateT state) (StoreT (Tuple gf s)) =
  pairing (\(Tuple a s1) f1 -> f a (f1 s1)) (state s) gf

-- | `ReaderT` pairs with `EnvT`.
readerEnv :: forall f g e. Pairing f g -> Pairing (ReaderT e f) (EnvT e g)
readerEnv pairing f (ReaderT reader) (EnvT (Tuple e gb)) =
  pairing f (reader e) gb

-- | `WriterT` pairs with `TracedT`.
writerTraced :: forall f g w. Pairing f g -> Pairing (WriterT w f) (TracedT w g)
writerTraced pairing f (WriterT writer) (TracedT gf) =
  pairing (\(Tuple a w) f1 -> f a (f1 w)) writer gf

-- | `Free` pairs with `Cofree`.
freeCofree :: forall f g. Functor f => Pairing f g -> Pairing (Free f) (Cofree g)
freeCofree pairing f free cofree =
  case resume free of
    Left fa -> pairing (freeCofree pairing f) fa (tail cofree)
    Right a -> f a (head cofree)
