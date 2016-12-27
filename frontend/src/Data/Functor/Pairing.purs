module Data.Functor.Pairing where

import Prelude

import Control.Comonad.Cofree (Cofree, explore)
import Control.Comonad.Env.Trans (EnvT(..))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Control.Monad.Free (Free)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))

import Data.Either (Either(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), uncurry)


class Pairing f g | f -> g, g -> f where
  pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c

zap :: forall f g a b. Pairing f g => f (a -> b) -> g a -> b
zap = pair ($)

-- | Pairing is symmetric
sym :: forall f g a b c. Pairing f g => (a -> b -> c) -> g a -> f b -> c
sym f ga fb = pair (flip f) fb ga

-- | The identity functor pairs with itself
instance pairingIdentity :: Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance pairingArrowTuple :: Pairing ((->) a) (Tuple a) where
  pair f t = uncurry (f <<< t)

instance pairingProd :: (Pairing f1 g1, Pairing f2 g2)
  => Pairing (Product f1 f2) (Coproduct g1 g2) where
    pair f (Product (Tuple f1 f2)) (Coproduct e) = case e of
      Left g1 -> pair f f1 g1
      Right g2 -> pair f f2 g2

instance pairingStateStore :: Pairing f g => Pairing (StateT s f) (StoreT s g) where
  pair f (StateT st) (StoreT (Tuple gf s)) =
    pair (\(Tuple a s1) f1 -> f a (f1 s1)) (st s) gf

instance pairingReaderEnv :: Pairing f g => Pairing (ReaderT e f) (EnvT e g) where
  pair f (ReaderT reader) (EnvT (Tuple e gb)) = pair f (reader e) gb

instance pairingwriterTraced :: Pairing f g => Pairing (WriterT w f) (TracedT w g) where
  pair f (WriterT writer) (TracedT gf) =  pair (\(Tuple a w) f1 -> f a (f1 w)) writer gf

instance pairingFreeCofree :: (Functor f, Functor g, Pairing f g) => Pairing (Free f) (Cofree g) where
  pair f free cofree = explore zap (map f free) cofree
