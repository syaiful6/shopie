module Shopie.NavMenu.Component.State
  ( NavMenuState
  , UserName
  , initialState
  , toggleAll
  , toggleUserMenu
  , toggleHelpMenu
  ) where

import Control.Semigroupoid ((<<<))

import Data.HeytingAlgebra (not)

import Shopie.Button.Dropdown (DropdownState(..))

type UserName = String

type NavMenuState =
  { username :: UserName
  , userMenu :: DropdownState
  , helpMenu :: DropdownState
  }

initialState ::  UserName -> NavMenuState
initialState username =
  { username: username
  , userMenu: DropdownClosed
  , helpMenu: DropdownClosed
  }

toggleAll :: NavMenuState -> NavMenuState
toggleAll = toggleUserMenu <<< toggleHelpMenu

toggleUserMenu :: NavMenuState -> NavMenuState
toggleUserMenu st@{ userMenu } = st { userMenu = not userMenu }

toggleHelpMenu :: NavMenuState -> NavMenuState
toggleHelpMenu st@{ helpMenu } = st { helpMenu = not helpMenu }
