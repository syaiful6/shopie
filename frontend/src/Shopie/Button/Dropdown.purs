module Shopie.Button.Dropdown
  ( DropdownState(..)
  , toggleDropdown
  , dropdownCSS
  ) where

import Prelude

-- a basic dropdown state
data DropdownState = DropdownClosed | DropdownOpen

derive instance eqDropdownState :: Eq DropdownState

toggleDropdown :: DropdownState -> DropdownState
toggleDropdown DropdownClosed = DropdownOpen
toggleDropdown DropdownOpen = DropdownClosed

dropdownCSS :: DropdownState -> String
dropdownCSS DropdownClosed = "closed"
dropdownCSS DropdownOpen = "open"
