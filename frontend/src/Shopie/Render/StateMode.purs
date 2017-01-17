module Shopie.Render.StateMode
  ( StateMode(..)
  , loadingRender
  , isLoading
  ) where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Qyson.Error (ErrorQ)

data StateMode
  = Loading
  | Ready
  | Error ErrorQ

isLoading :: StateMode -> Boolean
isLoading Loading = true
isLoading _ = false

loadingRender :: forall p i. H.HTML p i
loadingRender =
  HH.div
    [ HP.class_ $ HH.className "page-wrapper" ]
    [ HH.div
        [ HP.class_ $ HH.className "row" ]
        [ HH.div
            [ HP.class_ $ HH.className "spinner" ]
            [ HH.div
                [ HP.class_ $ HH.className "double-bounce1" ]
                []
            , HH.div
                [ HP.class_ $ HH.className "double-bounce2" ]
                []
            ]
        ]
    ]
