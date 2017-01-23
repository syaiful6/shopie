module Shopie.NavMenu.Component.Query
  ( NavMenuQuery(..)
  ) where


data NavMenuQuery a
  = Init a
  | CloseAll a
  | ToggleAll a
  | ToggleUserMenu a
  | ToggleHelpMenu a
