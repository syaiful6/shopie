module Shopie.NavMenu.Component
  ( NavMenuSlot(..)
  , navMenu
  ) where

import Shopie.Prelude

import Control.Monad.Aff.Bus as Bus

import Halogen as H
import Halogen.HTML.Indexed as HH

import Shopie.ShopieM (Shopie, Wiring(..))
import Shopie.Halogen.EventSource (raise)
import Shopie.NavMenu.Component.Render (renderUserMenu, renderNavBody)
import Shopie.NavMenu.Component.State (NavMenuState, toggleAll, toggleUserMenu, toggleHelpMenu)
import Shopie.NavMenu.Component.Query (NavMenuQuery(..))


data NavMenuSlot = NavMenuSlot
derive instance eqNavMenuSlot :: Eq NavMenuSlot
derive instance ordNavMenuSlot :: Ord NavMenuSlot

navMenu :: H.Component NavMenuState NavMenuQuery Shopie
navMenu = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render :: NavMenuState -> H.ComponentHTML NavMenuQuery
render s =
  HH.div_
    [ renderUserMenu s
    , renderNavBody s
    ]

eval :: NavMenuQuery ~> H.ComponentDSL NavMenuState NavMenuQuery Shopie
eval (Init _) = do
  Wiring { bodyClick } <- H.liftH ask
  forever (raise <<< ToggleAll =<< H.fromAff (Bus.read bodyClick))
eval (ToggleAll next) = H.modify toggleAll $> next
eval (ToggleUserMenu next) = H.modify toggleUserMenu $> next
eval (ToggleHelpMenu next) = H.modify toggleHelpMenu $> next
