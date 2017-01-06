module Shopie.Wiring where

import Prelude

import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff)

import Network.HTTP.Affjax as AX

import Shopie.Auth.Types (Oauth2Client)
import Shopie.Auth.AuthF.Interpreter.Wiring as IW
import Shopie.ShopieM.Notification (Notification)
import Shopie.Effects (ShopieEffects)
import Shopie.Route.Types (Locations)


type WiringR =
  { auth :: IW.Wiring ()
  , notify :: Bus.BusRW Notification
  , route :: Bus.BusRW Locations
  }

newtype Wiring = Wiring WiringR

unWiring :: Wiring -> WiringR
unWiring (Wiring r) = r

makeWiring
  :: forall m eff
   . (Affable (ShopieEffects eff) m)
  => AX.URL
  -> Oauth2Client
  -> m Wiring
makeWiring basePath oc = fromAff $
  { auth: _
  , notify: _
  , route: _
  }
  <$> ({ basePath: basePath
       , client: oc
       , signinBus: _
       , forgotBus: _
       } <$> Bus.make
         <*> Bus.make)
  <*> Bus.make
  <*> Bus.make
  <#> Wiring
