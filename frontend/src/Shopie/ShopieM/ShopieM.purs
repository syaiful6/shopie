module Shopie.ShopieM.ShopieM where

import Shopie.Prelude

import Control.Monad.Aff (Aff)
import Control.Applicative.Free (FreeAp, liftAp)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (tailRecM, Step(..))

import Qyson.ConfigF as QC
import Qyson.QysonF (QysonF)

import Shopie.Auth.AuthF as AT
import Shopie.Auth.Types (Email, TokenId)
import Shopie.Auth.Class (class AuthDSL)
import Shopie.Effects (ShopieEffects)
import Shopie.ShopieM.ForkF as SF
import Shopie.ShopieM.Notification (class NotifyQ, Notification)
import Shopie.Wiring (Wiring)


data ShopieF eff a
  = Aff (Aff eff a)
  | Halt String a
  | Par (ShopieAp eff a)
  | Notify Notification a
  | Fork (SF.Fork (ShopieM eff) a)

instance functorShopieF :: Functor (ShopieF eff) where
  map f = case _ of
    Aff aff -> Aff (map f aff)
    Halt s a -> Halt s (f a)
    Par sa -> Par (map f sa)
    Fork fa -> Fork (map f fa)
    Notify n a -> Notify n (f a)

type ShopieFC eff = Coproduct (QC.ConfigF Wiring) (Coproduct AT.AuthF (Coproduct QysonF (ShopieF eff)))

newtype ShopieM eff a = ShopieM (Free (ShopieFC eff) a)

type Shopie = ShopieM (ShopieEffects ())

instance functorShopieM :: Functor (ShopieM eff) where
  map f (ShopieM fa) = ShopieM $ map f fa

instance applyShopieM :: Apply (ShopieM eff) where
  apply (ShopieM fa) (ShopieM fb) = ShopieM (apply fa fb)

instance applicativeShopieM :: Applicative (ShopieM eff) where
  pure a = ShopieM (pure a)

instance bindShopieM :: Bind (ShopieM eff) where
  bind (ShopieM fa) f = ShopieM (fa >>= \x -> case f x of ShopieM fb -> fb)

instance monadShopieM :: Monad (ShopieM eff)

instance monadRecShopieM :: MonadRec (ShopieM eff) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadEffShopieM :: MonadEff eff (ShopieM eff) where
  liftEff = ShopieM <<< liftF <<< right <<< right <<< right <<< Aff <<< liftEff

instance monadAffShopieM :: MonadAff eff (ShopieM eff) where
  liftAff = ShopieM <<< liftF <<< right <<< right <<< right <<< Aff <<< liftAff

instance affableShopieM :: Affable eff (ShopieM eff) where
  fromAff = ShopieM <<< liftF <<< right <<< right <<< right <<< Aff

instance parallelShopieM :: Parallel (ShopieAp eff) (ShopieM eff) where
  parallel = ShopieAp <<< liftAp
  sequential = ShopieM <<< liftF <<< right <<< right <<< right <<< Par

instance monadForkShopieM :: MonadAff eff m => MonadFork Error (ShopieM eff) where
  fork a = map liftAff <$> ShopieM (liftF $ right $ right $ right $ Fork $ SF.fork a)

instance monadAskShopieM :: MonadAsk Wiring (ShopieM eff) where
  ask = ShopieM $ liftF $ left $ QC.configF id

instance notifyQShopieM :: NotifyQ (ShopieM eff) where
  notify = ShopieM <<< liftF <<< right <<< right <<< right <<< flip Notify unit

instance authDSLShopieM :: AuthDSL (ShopieM eff) where
  authenticate = ShopieM <<< liftF <<< right <<< left <<< AT.authenticateF
  maybeAuthId = ShopieM $ liftF $ right $ left $ AT.maybeAuthIdF
  invalidate = ShopieM $ liftF $ right $ left $ AT.invalidateF

newtype ShopieAp eff a = ShopieAp (FreeAp (ShopieM eff) a)

derive newtype instance functorShopieAp :: Functor (ShopieAp eff)
derive newtype instance applyShopieAp :: Apply (ShopieAp eff)
derive newtype instance applicativeShopieAp :: Applicative (ShopieAp eff)

unShopieM :: forall eff. ShopieM eff ~> Free (ShopieFC eff)
unShopieM (ShopieM m) = m

forgotten :: forall eff. Email -> ShopieM eff Unit
forgotten = ShopieM <<< liftF <<< right <<< left <<< AT.forgottenF

getAuthTokenId :: forall eff. ShopieM eff (Maybe TokenId)
getAuthTokenId = ShopieM $ liftF $ right $ left $ AT.getAuthTokenId

liftQyson :: forall eff. QysonF ~> ShopieM eff
liftQyson = ShopieM <<< liftF <<< right <<< right <<< left
