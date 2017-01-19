module Shopie.NavMenu.Component.Render where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH

import Shopie.Button.Dropdown (dropdownCSS)
import Shopie.NavMenu.Component.State (NavMenuState)
import Shopie.NavMenu.Component.Query (NavMenuQuery)


-- | render user dropdown menu
renderUserMenu :: NavMenuState -> H.ComponentHTML NavMenuQuery
renderUserMenu { username, userMenu } =
  HH.div_
    [ HH.header
        [ HP.class_ $ HH.className "sh-nav-menu " <> dropdownCSS userMenu ]
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
                [ HH.text username ]
            ]
        ]
    , HH.div
        [ HP.class_ $ HH.className "dropdown " <> dropdownCSS userMenu ]
        [ HH.ul
            [ HP.class_ $ HH.className dropdownItemCSS
            , HPA.role "menu"
            ]
            [ HH.li
                [ HPA.role "presentation" ]
                [ HH.i
                    [ HP.class_ $ HH.className "icon-shop" ]
                , HH.text "About Shopie"
                ]
            , HH.li
                [ HH.class_ $ HH.className "divider" ]
                [ ]
            , HH.li
                [ HPA.role "presentation" ]
                [ HH.a
                    [ HP.class_ $ HH.className "dropdown-item user-menu-profile js-nav-item"
                    , HPA.role "menuitem"
                    ]
                    [ HH.i [ HP.class_ $ HH.className "icon-user"] [ ]
                    , HH.text "Your Profile"
                    ]
                ]
            , HH.li
                [ HPA.role "presentation" ]
                [ HH.a
                    [ HP.class_ $ HH.className "dropdown-item user-menu-signout"
                    , HPA.role "menuitem"
                    ]
                    [ HH.i [ HP.class_ $ HH.className "icon-signout"] [ ]
                    , HH.text "Sign out"
                    ]
                ]
            ]
        ]
    ]
  where
    dropdownItemCSS = "dropdown-menu dropdown-triangle-top js-user-menu-dropdown-menu"

renderNavBody :: NavMenuState -> H.ComponentHTML NavMenuQuery
renderNavBody s =
  HH.section
    [ HP.class_ $ HH.className "sh-nav-body" ]
    []
