module Shopie.Auth.AuthF.Interpreter.Affable
  ( module Shopie.Auth.AuthF.Interpreter.Wiring
  , eval
  ) where

import Prelude

import Control.Applicative.Lift (Lift(..))

import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Functor.Coproduct (coproduct)

import Qyson.ConfigF as QF

import Network.HTTP.AffjaxF as AXF

import Shopie.Auth.AuthF (AuthF)
import Shopie.Auth.AuthF.Interpreter.Internal (AuthEffect)
import Shopie.Auth.AuthF.Interpreter.Wiring (Wiring, SignInMessage(..), ForgotMessage(..))
import Shopie.Auth.StorageF.Interpreter.Affable as SIA
import Shopie.Auth.AuthF.Interpreter.Affjax as AIA


eval
  :: forall g m eff
   . ( MonadReader (Wiring g) m
     , Affable (AuthEffect eff) m
     , MonadRec m
     )
  => AuthF
  ~> m
eval =
  foldFree
  (coproduct
    QF.evalReader
    (coproduct
      (fromAff <<< AXF.eval)
      (coproduct SIA.eval runLift)
    )
  )
  <<< AIA.eval

runLift :: forall f. Applicative f => Lift f ~> f
runLift (Lifted f) = f
runLift (Pure a) = pure a
