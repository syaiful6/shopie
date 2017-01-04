module Shopie.Auth.StorageF.Interpreter.Affable where

import Prelude

import Control.Monad.Aff.Free (class Affable, fromEff)

import Shopie.Auth.StorageF (StorageF)
import Shopie.Auth.StorageF.Interpreter.Eff as IE

import DOM (DOM)


eval
  :: forall eff m
   . Affable (dom :: DOM | eff) m
  => StorageF
  ~> m
eval = fromEff <<< IE.eval
