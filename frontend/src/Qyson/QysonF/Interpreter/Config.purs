module Qyson.QysonF.Interpreter.Config where

import Network.HTTP.Affjax as AX


type Config r =
  { basePath :: AX.URL
  | r
  }
