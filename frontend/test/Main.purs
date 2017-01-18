module Test.Main where

import Prelude

import Control.Monad.Aff.MVar (MVAR)
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (QC)

import Test.Shopie.Form.Field as SFF
import Test.Shopie.Form.Usage as FU
import Test.MVar as TM

main :: forall e. QC (mvar :: MVAR | e) Unit
main = do
  log "test form field"
  SFF.main

  log "test form usage"
  FU.main

  TM.main
