module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC)

import Test.Shopie.Form.Field as SFF

main :: forall e. QC e Unit
main = do
  log "test form field"
  SFF.main
