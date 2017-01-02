module Qyson.Data
  ( JSONMode
  , DataQ
  , jsonModeApi
  , jsonModeNormal
  , jsonApi
  , json
  , urlEncoded
  , customData
  , applicationVndJSONApi
  , jsonMediaType
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.FormURLEncoded as FU
import Data.Maybe (Maybe(Just))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..), snd)

import Network.HTTP.Affjax.Request (class Requestable, toRequest)


data JSONMode
  = ModeApi -- vnd json api
  | ModeNormal

data DataQ
  = JSONData JSONMode Json
  | FormURLEncoded FU.FormURLEncoded
  | Custom MediaType String

instance requestableDataQ :: Requestable DataQ where
  toRequest (JSONData ModeApi c) =
    Tuple
      (Just applicationVndJSONApi)
      (snd (toRequest c))
  toRequest (JSONData _ j) = toRequest j
  toRequest (FormURLEncoded fm) = toRequest fm
  toRequest (Custom mt str) =
    Tuple (Just mt) (snd (toRequest str))

jsonModeApi :: JSONMode
jsonModeApi = ModeApi

jsonModeNormal :: JSONMode
jsonModeNormal = ModeNormal

jsonApi :: Json -> DataQ
jsonApi = JSONData jsonModeApi

json :: Json -> DataQ
json = JSONData jsonModeNormal

urlEncoded :: Array (Tuple String (Maybe String)) -> DataQ
urlEncoded = FormURLEncoded <<< FU.fromArray

customData :: MediaType -> String -> DataQ
customData = Custom

applicationVndJSONApi :: MediaType
applicationVndJSONApi = MediaType $ "application/vnd.api+json"

jsonMediaType :: JSONMode -> MediaType
jsonMediaType ModeApi = applicationVndJSONApi
jsonMediaType ModeNormal = applicationJSON
