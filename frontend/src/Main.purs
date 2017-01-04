module Main where

import Shopie.Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)

import Shopie.Application as App
import Shopie.Auth.Types (Oauth2Client(..), readClient)
import Shopie.ShopieM (ShopieEffects, makeWiring)
import Shopie.ShopieM.Interpreter.Aff (runShopieM)

main :: Eff (ShopieEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  oauth <- liftEff createOauth
  wiring <- makeWiring "" oauth
  let app' = H.interpret (runShopieM wiring) App.app
  driver <- H.runUI app' (H.parentState App.initialState) body
  -- TODO setup Route Signal
  pure unit

createOauth :: Eff (ShopieEffects ()) Oauth2Client
createOauth = do
  rc <- readClient
  let empty =
        { clientId: ""
        , clientSecret: ""
        }
  pure $ fromMaybe (Oauth2Client empty) rc
