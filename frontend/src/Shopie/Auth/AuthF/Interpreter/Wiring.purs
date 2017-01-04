module Shopie.Auth.AuthF.Interpreter.Wiring
  ( SignInMessage(..)
  , ForgotMessage(..)
  , Wiring
  ) where

import Network.HTTP.Affjax as AX

import Control.Monad.Aff.Bus (BusRW)

import Shopie.Auth.Types (Oauth2Client)

data SignInMessage = SignInSuccess | SignInFailure | SignOutRequest

data ForgotMessage = ForgotSucces | ForgotFailure

type Wiring r =
  { basePath :: AX.URL
  , client :: Oauth2Client
  , signinBus :: BusRW SignInMessage
  , forgotBus :: BusRW ForgotMessage
  | r
  }
