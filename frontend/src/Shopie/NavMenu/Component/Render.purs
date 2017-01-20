module Shopie.NavMenu.Component.Render where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH

import Data.Maybe (Maybe(..))

import Shopie.Button.Dropdown (dropdownCSS)
import Shopie.NavMenu.Component.State (NavMenuState)
import Shopie.NavMenu.Component.Query (NavMenuQuery(..))


-- | render user dropdown menu
renderUserMenu :: NavMenuState -> H.ComponentHTML NavMenuQuery
renderUserMenu { username, userMenu } =
  HH.div_
    [ HH.header
        [ HP.class_ $ HH.className $ "sh-nav-menu " <> dropdownCSS userMenu
        , HE.onClick (\_ -> HEH.stopPropagation $> Just (H.action ToggleUserMenu))
        ]
        [ HH.div
            [ HP.class_ $ HH.className "sh-nav-menu-icon" ]
            []
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
        [ HP.class_ $ HH.className $ "dropdown " <> dropdownCSS userMenu ]
        [ HH.ul
            [ HP.class_ $ HH.className dropdownItemCSS ]
            [ HH.li
                []
                [ HH.i [ HP.class_ $ HH.className "icon-shop" ] []
                , HH.text "About Shopie"
                ]
            , HH.li
                [ HP.class_ $ HH.className "divider" ]
                []
            , HH.li
                []
                [ HH.a
                    [ HP.class_ $ HH.className "dropdown-item user-menu-profile js-nav-item" ]
                    [ HH.i [ HP.class_ $ HH.className "icon-user"] []
                    , HH.text "Your Profile"
                    ]
                ]
            , HH.li
                []
                [ HH.a
                    [ HP.class_ $ HH.className "dropdown-item user-menu-signout" ]
                    [ HH.i [ HP.class_ $ HH.className "icon-signout"] []
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
    [ HH.ul
        [ HP.class_ $ HH.className "sh-nav-list sh-nav-settings" ]
        [ HH.li_
            [ HH.i [ HP.class_ $ HH.className "icon-dash" ] []
            , HH.text "Settings"
            ]
        , HH.li_
            [ HH.a
                [ HP.class_ $ HH.className "sh-nav-main-users" ]
                [ HH.i [ HP.class_ $ HH.className "icon-team" ] []
                , HH.text "Users"
                ]
            ]
        ]
    ]
