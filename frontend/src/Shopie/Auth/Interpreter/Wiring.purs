module Shopie.Auth.Interpreter.Wiring
  ( SignInMessage(..)
  , ForgotMessage(..)
  , Wiring
  ) where

import Network.HTTP.Affjax as AX

import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Aff.Bus (BusRW)

import Shopie.Auth.Types (Oauth2Client)

data SignInMessage = SignInSuccess | SignInFailure | SignOutRequest

data ForgotMessage = ForgotSucces | ForgotFailure

type Wiring r =
  { endpoint :: AX.URL
  , revokeEndPoint :: AX.URL
  , client :: Ref Oauth2Client
  , signinBus :: BusRW SignInMessage
  , forgotBus :: BusRW ForgotMessage
  | r
  }
