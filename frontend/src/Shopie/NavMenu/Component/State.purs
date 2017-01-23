module Shopie.NavMenu.Component.State
  ( NavMenuState
  , UserName
  , makeNavState
  , closeAll
  , toggleAll
  , toggleUserMenu
  , toggleHelpMenu
  ) where

import Control.Semigroupoid ((<<<))

import Data.HeytingAlgebra (not, ff)

import Shopie.Button.Dropdown (DropdownState)

type UserName = String

type NavMenuState =
  { username :: UserName
  , userMenu :: DropdownState
  , helpMenu :: DropdownState
  }

makeNavState ::  UserName -> NavMenuState
makeNavState username =
  { username: username
  , userMenu: ff
  , helpMenu: ff
  }

closeAll :: NavMenuState -> NavMenuState
closeAll st@{ userMenu, helpMenu } = st { userMenu = ff, helpMenu = ff }

toggleAll :: NavMenuState -> NavMenuState
toggleAll = toggleUserMenu <<< toggleHelpMenu

toggleUserMenu :: NavMenuState -> NavMenuState
toggleUserMenu st@{ userMenu } = st { userMenu = not userMenu }

toggleHelpMenu :: NavMenuState -> NavMenuState
toggleHelpMenu st@{ helpMenu } = st { helpMenu = not helpMenu }
