module Qyson.QysonF.Interpreter.Aff
  ( eval
  , module Qyson.QysonF.Interpreter.Config
  ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Functor.Coproduct (coproduct)

import Network.HTTP.Affjax as AX
import Network.HTTP.AffjaxF as AXF

import Qyson.ConfigF as CF
import Qyson.QysonF (QysonF)
import Qyson.QysonF.Interpreter.Affjax as IAX
import Qyson.QysonF.Interpreter.Config (Config)


eval
  :: forall m eff r
   . ( MonadReader { basePath :: AX.URL | r } m
     , MonadAff (ajax :: AX.AJAX | eff) m
     , MonadRec m
     )
  => QysonF
  ~> m
eval = foldFree (coproduct CF.evalReader (liftAff <<< AXF.eval)) <<< IAX.eval
