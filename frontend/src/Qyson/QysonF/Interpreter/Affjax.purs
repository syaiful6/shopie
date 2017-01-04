module Qyson.QysonF.Interpreter.Affjax
  ( FreeQCF
  , eval
  , module Qyson.QysonF.Interpreter.Config
  ) where

import Prelude

import Control.Monad.Free (Free)

import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.HTTP.Method (Method(..))
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (currentDir)
import Data.StrMap as SM
import Data.Tuple (snd, fst)

import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req

import Qyson.ConfigF as CF
import Qyson.QysonF (QysonF(..), jsonMediaType)
import Qyson.QysonF.Interpreter.Config (Config)
import Qyson.QysonF.Interpreter.Internal (mkURL, deleteReq, mkRequest, defaultRequest,
                                          jsonResult, toPageParams, unitResult)


type FreeQCF r = Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String))

eval :: forall r. QysonF ~> FreeQCF r
eval = case _ of

  ReadQuery mode dp vp mp k -> do
    let params = SM.toList vp <> toPageParams mp
    url <- mkURL currentDir (Left dp) params
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , method = Left GET
        , headers = [Req.Accept $ jsonMediaType mode]
        })

  WriteQuery path content k -> do
    url <- mkURL currentDir (Left path) Nil
    let reqSettings = toRequest content
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , method = Left PUT
        , headers = catMaybes [ Req.Accept <$> fst reqSettings
                              , Req.ContentType <$> fst reqSettings
                              ]
        , content = Just $ snd reqSettings
        })

  ReadFile mode fp m k -> do
    url <- mkURL currentDir (Right fp) (toPageParams m)
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , method = Left GET
        , headers = [Req.Accept $ jsonMediaType mode]
        })

  WriteFile path content k -> do
    url <- mkURL currentDir (Right path) Nil
    let reqSettings = toRequest content
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , headers = catMaybes [ Req.Accept <$> fst reqSettings
                              , Req.ContentType <$> fst reqSettings
                              ]
        , method = Left PUT
        , content = Just $ snd reqSettings
        })

  AppendFile path content k -> do
    url <- mkURL currentDir (Right path) Nil
    let reqSettings = toRequest content
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , headers = catMaybes [ Req.Accept <$> fst reqSettings
                              , Req.ContentType <$> fst reqSettings
                              ]
        , method = Left POST
        , content = Just $ snd reqSettings
        })

  DeleteFile pt k -> do
    k <$> (mkRequest unitResult <<< deleteReq =<< mkURL currentDir pt Nil)
