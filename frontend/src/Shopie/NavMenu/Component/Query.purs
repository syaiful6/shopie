module Shopie.NavMenu.Component.Query
  ( NavMenuQuery(..)
  ) where


data NavMenuQuery a
  = Init a
  | ToggleUserMenu Boolean a
  | ToggleHelpMenu Boolean a
