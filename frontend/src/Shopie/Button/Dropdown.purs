module Shopie.Button.Dropdown
  ( DropdownState(..)
  , toggleDropdown
  , dropdownCSS
  ) where

import Prelude

-- a basic dropdown state
data DropdownState = DropdownOpen | DropdownClosed

toggleDropdown :: DropdownState -> DropdownState
toggleDropdown DropdownClosed = DropdownOpen
toggleDropdown DropdownOpen = DropdownClosed

dropdownCSS :: DropdownState -> String
dropdownCSS DropdownClosed = "closed"
dropdownCSS DropdownOpen = "open"

derive instance eqDropdownState :: Eq DropdownState

instance ordDropdownState :: Ord DropdownState where
  compare DropdownOpen DropdownClosed = GT
  compare DropdownClosed DropdownOpen = LT
  compare _ _ = EQ

instance showDropDownState :: Show DropdownState where
  show DropdownClosed = "DropdownClosed"
  show DropdownOpen = "DropdownOpen"

instance heytingAlgebraDropdownState :: HeytingAlgebra DropdownState where
  ff = DropdownClosed
  tt = DropdownOpen
  implies a b = not a || b
  conj DropdownOpen DropdownOpen = DropdownOpen
  conj _ _ = DropdownClosed
  disj DropdownOpen _ = DropdownOpen
  disj _ DropdownOpen = DropdownOpen
  disj _ _ = DropdownClosed
  not DropdownClosed = DropdownOpen
  not DropdownOpen = DropdownClosed

instance booleanAlgebraDropdownState :: BooleanAlgebra DropdownState
