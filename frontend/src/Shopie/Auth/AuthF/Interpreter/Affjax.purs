module Shopie.Auth.Interpreter.OAjax where

import Prelude

import Control.Applicative.Lift (Lift)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromEff, fromAff)
import Control.Monad.Eff.Ref (Ref, readRef, writeRef)
import Control.Monad.Free (Free)

import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Tuple (snd, fst)

import Network.HTTP.Affjax (AffjaxResponse, defaultRequest)
import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader (RequestHeader(..))

import Data.Array (catMaybes)
import Qyson.Data (jsonApi)

import Shopie.Auth.AuthF (AuthF)
import Shopie.Auth.Interpreter.Internal as AI
import Shopie.Auth.Interpreter.Wiring (Wiring, SignInMessage(..), ForgotMessage(..))
import Shopie.Auth.Types (UserId)


type CP3 a b c d = Coproduct a (Coproduct b (Coproduct c d))

type M g m = Free (CP3
                    (CF.ConfigF (Wiring g))
                    (AXF.AffjaxFP RequestContent String)
                    SF.StorageF
                    (Lift m)
                  )

eval :: forall g m. Affable m => AuthF ~> M g m
eval = case _ of

  Authenticate creds k -> do
    w@{ basePath, client } <- AI.ask
    let reqSettings = toRequest $ AI.authenticateBody client creds
    res <- AI.normalizeExpiration <$> mkRequest AI.bearerResult
      (AXF.affjax $ defaultRequest
        { url = basePath <> "/o/token/"
        , headers = catMaybes $ [ Accept <$> fst reqSettings
                                , ContentType <$> fst reqSettings
                                ]
        , content = Just $ snd reqSettings
        })
    case res of
      Right b -> do
        IA.persistBearerToken storageK b
        IA.lift $ fromAff $ Bus.write SignInSuccess w.signinBus
        pure $ k $ Authenticated "authenticated success"
      Left _ -> do
        IA.lift $ fromAff $ Bus.write AuthFailure auth
        pure $ k $ ServerError "authenticated failure"

  MaybeAuthId k ->
    (map getUserId >=> pure <<< k) =<< IA.restoreBearerToken storageK

getUserId :: forall g m. BearerToken -> M g m (Maybe UserId)
getUserId (BearerToken t) = do
  { basePath } <- AI.ask
  hust <$> mkRequest AI.userIdResult
    (AXF.Affjax $ defaultRequest
      { url = basePath <> "/api/v1/users/me/"
      , headers = [Accept jsonApi, RequestHeader "Authorization" (t.accessToken)]
      })

storageK :: String
storageK = "shopie:auth-storage"
