module Shopie.Utils.DOM
  ( bodyClick
  , locationObj
  ) where

import Prelude

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (Location, htmlDocumentToEventTarget)
import DOM.HTML.Window (document, location)


-- | create aff that listen to event listener
bodyClick :: forall eff. Aff (dom :: DOM | eff) Unit
bodyClick = makeAff \_ k -> liftEff $
  window >>=
    (document >=> htmlDocumentToEventTarget
      >>> addEventListener click (eventListener (\_ -> k unit)) false)

locationObj :: forall eff. Eff (dom :: DOM | eff) Location
locationObj = window >>= location
