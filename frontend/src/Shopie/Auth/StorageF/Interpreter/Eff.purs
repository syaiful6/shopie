module Shopie.Auth.StorageF.Interpreter.Eff where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Error.Util (hush)

import Shopie.Auth.StorageF (StorageF(..))
import Shopie.Utils.Storage as US

import DOM (DOM)


eval :: forall eff. StorageF ~> Eff (dom :: DOM | eff)
eval (Persist k v next) =
  US.setLocalStorage k v $> next

eval (Restore key kon) =
  kon <<< hush <$> US.getLocalStorage key

eval (Remove key next) =
  US.removeLocalStorage key $> next
