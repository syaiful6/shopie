module Shopie.Auth.AuthF.Interpreter.Wiring
  ( SignInMessage(..)
  , ForgotMessage(..)
  , Wiring
  ) where

import Prelude

import Control.Monad.Aff.Bus (BusRW)

import Network.HTTP.Affjax as AX

import Shopie.Auth.Types (Oauth2Client)


data SignInMessage = SignInSuccess | SignInFailure | SignOutRequest

data ForgotMessage = ForgotSucces | ForgotFailure

derive instance eqForgotMessage :: Eq ForgotMessage

derive instance eqSignInMessage :: Eq SignInMessage

type Wiring r =
  { basePath :: AX.URL
  , client :: Oauth2Client
  , signinBus :: BusRW SignInMessage
  , forgotBus :: BusRW ForgotMessage
  | r
  }
