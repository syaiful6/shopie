module Shopie.ShopieM.Interpreter.Aff where

import Shopie.Prelude

import Control.Applicative.Free (hoistAp, restractAp)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Fork (fork)
import Control.Monad.Reader (runReaderT)

import Qyson.ConfigF as QC

import Shopie.Auth.AuthF.Interpreter.Affable as AIA
import Shopie.Effects (ShopieEffects)
import Shopie.ShopieM.ForkF as SF
import Shopie.ShopieM.ShopieM (ShopieF(..), ShopieFC, ShopieM, ShopieAp(..), unShopieM)
import Shopie.Wiring (Wiring(..))


runShopieM
  :: forall eff
   . Wiring
  -> ShopieM (ShopieEffects eff)
  ~> Aff (ShopieEffects eff)
runShopieM wiring@(Wiring { auth, notify }) = foldFree go <<< unShopieM
  where
    go :: ShopieFC (ShopieEffects eff) ~> Aff (ShopieEffects eff)
    go =
      coproduct
        (flip QC.evalAp wiring)
        (coproduct (flip runReaderT auth <<< AIA.eval) evalShopieF)

    evalShopieF :: ShopieF (ShopieEffects eff) ~> Aff (ShopieEffects eff)
    evalShopieF = case _ of
      Aff aff ->
        aff
      Halt s next ->
        pure next
      Par (ShopieAp p) ->
        sequential $ restractAp $ hoistAp (parallel <<< runShopieM wiring) p
      Fork f ->
        goFork f
      Notify msg a ->
        Bus.write msg notify $> a

    goFork :: SF.Fork (ShopieM (ShopieEffects eff)) ~> Aff (ShopieEffects eff)
    goFork = SF.unFork \(SF.ForkF fx k) ->
      k <<< map unsafeCoerceAff <$> fork (runShopieM wiring fx)
