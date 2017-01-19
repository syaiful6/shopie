module Shopie.NavMenu.Component  where

import Shopie.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH

import Shopie.ShopieM (Shopie)
import Shopie.Button.Dropdown (DropdownState(..), dropdownCSS)
import Shopie.NavMenu.Component.State (NavMenuState)
import Shopie.NavMenu.Component.Query (NavMenuQuery(..))


navMenu :: H.Component NavMenuState NavMenuQuery Shopie
navMenu = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render :: NavMenuState -> H.ComponentHTML NavMenuQuery
render { user, userMenu, helpMenu } =
  HH.header
    [ HP.class_ $ HH.className "sh-nav-menu dropdown " <> dropdownCSS userMenu ]
    [ HH.div
        [ HP.class_ $ HH.className "sh-nav-menu-icon" ]
        [ ]
    , HH.div
        [ HP.class_ $ HH.className "sh-nav-menu-details" ]
        [ HH.div
            [ HP.class_ $ HH.className "sh-nav-menu-details-shop" ]
            [ HH.text "Shopie" ]
        , HH.div
            [ HP.class_ $ HH.className "sh-nav-menu-details-user" ]
            [ HH.text user.username ]
        ]
    ]
