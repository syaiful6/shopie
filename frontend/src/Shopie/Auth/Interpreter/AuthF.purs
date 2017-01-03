module Shopie.Auth.Interpreter.AuthF where

import Prelude

import Control.Monad.Free (Free)

import Data.Functor.Coproduct (Coproduct)

import Shopie.Auth.AuthF (AuthF)
import Shopie.Auth.Interpreter.Internal (ask, M)
import Shopie.Auth.Interpreter.Wiring (Wiring, SignInMessage(..), ForgotMessage(..))

eval :: forall g m. AuthF ~> M g m
eval = case _ of

  Authenticate creds k -> do
    { endpoint, client } <- ask
