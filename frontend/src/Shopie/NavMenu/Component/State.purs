module Shopie.NavMenu.Component.State
  ( NavMenuState
  , initialState
  ) where

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
