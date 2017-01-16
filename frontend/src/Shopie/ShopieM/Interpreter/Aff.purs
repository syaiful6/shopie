module Shopie.ShopieM.Interpreter.Aff where

import Shopie.Prelude

import Control.Applicative.Free (hoistAp, restractAp)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Fork (fork)
import Control.Monad.Reader (runReaderT)

import Network.HTTP.AffjaxF as AXF
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.RequestHeader as RH

import Qyson.QysonF (QysonF)
import Qyson.ConfigF as QC
import Qyson.QysonF.Interpreter.Affjax as IAX

import Shopie.Auth.AuthF.Interpreter.Affable as AIA
import Shopie.Auth.Class (invalidate)
import Shopie.Auth.Types (TokenId(..))
import Shopie.Effects (ShopieEffects)
import Shopie.ShopieM.ForkF as SF
import Shopie.ShopieM.ShopieM (ShopieF(..), ShopieFC, ShopieM, ShopieAp(..), unShopieM,
  getAuthTokenId)
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
        (coproduct
          (flip runReaderT auth <<< AIA.eval)
          (coproduct evalQyson evalShopieF)
        )

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

    evalQyson :: QysonF ~> Aff (ShopieEffects eff)
    evalQyson =
      foldFree (coproduct (flip QC.evalAp { basePath: "/api/v1"}) authify) <<< IAX.eval

    authify :: AXF.AffjaxFP RequestContent String ~> Aff (ShopieEffects eff)
    authify ajax@(AXF.AffjaxFP req k) = do
      token <- runShopieM wiring getAuthTokenId
      case token of
        Nothing ->
          runShopieM wiring invalidate *> AXF.eval ajax
        Just tok ->
          let req' = AXF.AffjaxFP (req { headers = req.headers <> [authHeader tok] }) k
          in AXF.eval req'

    goFork :: SF.Fork (ShopieM (ShopieEffects eff)) ~> Aff (ShopieEffects eff)
    goFork = SF.unFork \(SF.ForkF fx k) ->
      k <<< map unsafeCoerceAff <$> fork (runShopieM wiring fx)

authHeader :: TokenId -> RH.RequestHeader
authHeader (TokenId tok) =
  RH.RequestHeader "Authorization" ("Bearer " <> tok)
