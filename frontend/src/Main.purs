module Main where

import Shopie.Prelude

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)

import Routing as R

import Shopie.Application as App
import Shopie.Auth.Types (Oauth2Client(..), readClient)
import Shopie.Effects (RawShopieEffects)
import Shopie.ShopieM (ShopieEffects, makeWiring)
import Shopie.ShopieM.Interpreter.Aff (runShopieM)
import Shopie.Route (routing)
import Shopie.Route.Types (Locations)


main :: Eff (ShopieEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  oauth <- liftEff createOauth
  wiring <- makeWiring "" oauth
  let app' = H.interpret (runShopieM wiring) App.app
  driver <- H.runUI app' (H.parentState App.initialState) body
  forkAff (routeSignal driver)
  pure unit

routeSignal :: forall eff. H.Driver App.AppQP (RawShopieEffects eff) -> Aff (ShopieEffects eff) Unit
routeSignal driver = do
  t <- R.matchesAff routing
  uncurry (redirects driver) t

redirects
  :: forall eff
   . H.Driver App.AppQP (RawShopieEffects eff)
  -> Maybe Locations
  -> Locations
  -> Aff (ShopieEffects eff) Unit
redirects driver _ n =
  driver $ H.action $ left <<< App.Move n

createOauth :: Eff (ShopieEffects ()) Oauth2Client
createOauth = do
  rc <- readClient
  let empty =
        { clientId: ""
        , clientSecret: ""
        }
  pure $ fromMaybe (Oauth2Client empty) rc
