module Shopie.Auth.AuthF.Interpreter.Affjax
  ( module Shopie.Auth.AuthF.Interpreter.Wiring
  , CP3
  , M
  , eval
  ) where

import Shopie.Prelude

import Control.Applicative.Lift (Lift)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (fromEff, fromAff)
import Control.Error.Util (hush)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Free (Free)

import Data.DateTime.Instant (unInstant)

import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON, applicationFormURLEncoded)
import Network.HTTP.RequestHeader (RequestHeader(..))

import Data.Array (catMaybes)
import Qyson.ConfigF as CF
import Qyson.Data (applicationVndJSONApi, urlEncoded)

import Shopie.Auth.AuthF (AuthF(..))
import Shopie.Auth.StorageF (StorageF)
import Shopie.Auth.AuthF.Interpreter.Internal as AI
import Shopie.Auth.AuthF.Interpreter.Wiring (Wiring, SignInMessage(..), ForgotMessage(..))
import Shopie.Auth.Types (UserId, TokenId(..), BearerToken(..), AuthResult(..))


type CP3 a b c d = Coproduct a (Coproduct b (Coproduct c d))

type M g m = Free (CP3
                    (CF.ConfigF (Wiring g))
                    (AXF.AffjaxFP RequestContent String)
                    StorageF
                    (Lift m)
                  )

eval
  :: forall g m eff
   . Affable (avar :: AVAR, now :: NOW | eff) m
  => AuthF
  ~> M g m
eval = case _ of

  Authenticate creds k -> do
    w@{ basePath, client } <- AI.ask
    let reqSettings = toRequest $ AI.authenticateBody client creds
    res <- AI.mkRequest AI.bearerResult
      (AXF.affjax $ AI.defaultRequest
        { url = basePath <> "/o/token/"
        , method = Left POST
        , headers = [ Accept applicationJSON
                    , ContentType applicationFormURLEncoded
                    ]
        , content = Just $ snd reqSettings
        })
    case res of
      Right b -> do
        AI.persistBearerToken storageK (AI.normalizeExpiration b)
        AI.lift $ fromAff $ Bus.write SignInSuccess w.signinBus
        pure $ k $ Authenticated "authenticated success"
      Left _ -> do
        AI.lift $ fromAff $ Bus.write SignInFailure w.signinBus
        pure $ k $ ServerError "authenticated failure"

  MaybeAuthId k -> do
    d <- AI.restoreBearerToken storageK >>= maybe (pure Nothing) getUserId
    pure (k d)

  Invalidate k -> do
    { signinBus } <- AI.ask
    AI.remove storageK
    AI.lift $ fromAff $ Bus.write SignOutRequest signinBus
    pure $ k $ Invalidated "invalidated success!"

  Forgotten email next -> do
    { basePath, forgotBus } <- AI.ask
    let reqSettings = toRequest $ urlEncoded [Tuple "email" (Just email)]
    res <- AI.mkRequest (const (Right unit))
      (AXF.affjax $ AI.defaultRequest
        { url = basePath <> "/api/v1/users/passwordreset/"
        , method = Left POST
        , headers = catMaybes $ [ Accept <$> fst reqSettings
                                , ContentType <$> fst reqSettings
                                ]
        , content = Just $ snd reqSettings
        })
    AI.lift $ fromAff $ case res of
      Right _ -> Bus.write ForgotSucces forgotBus
      Left _ -> Bus.write ForgotFailure forgotBus
    pure next

  GetAuthTokenId k -> do
    token <- AI.restoreBearerToken storageK
    case token of
      Just bt@(BearerToken t) -> do
        n <- AI.lift $ fromEff $ map (unwrap <<< unInstant) now
        if t.expiresIn > n
          then pure $ k $ Just $ TokenId t.accessToken
          else refreshToken k bt
      Nothing -> pure (k Nothing)

refreshToken :: forall g m a. (Maybe TokenId -> a) -> BearerToken -> M g m a
refreshToken k tok = do
  { basePath, client } <- AI.ask
  let reqSettings = toRequest $ AI.refreshTokenBody client tok
  res <- AI.mkRequest AI.bearerResult
    (AXF.affjax $ AI.defaultRequest
      { url = basePath <> "/o/token/"
      , method = Left POST
      , headers = catMaybes $ [ Accept <$> fst reqSettings
                              , ContentType <$> fst reqSettings
                              ]
      , content = Just $ snd reqSettings
      })
  case res of
    Right bt@(BearerToken { accessToken }) -> do
      AI.persistBearerToken storageK (AI.normalizeExpiration bt)
      pure $ k $ Just $ TokenId accessToken
    Left _ -> pure $ k Nothing

getUserId :: forall g m. BearerToken -> M g m (Maybe UserId)
getUserId (BearerToken t) = do
  { basePath } <- AI.ask
  hush <$> AI.mkRequest AI.userIdResult
    (AXF.affjax $ AI.defaultRequest
      { url = basePath <> "/api/v1/users/me/"
      , method = Left GET
      , headers = [ Accept applicationVndJSONApi
                  , RequestHeader "Authorization" ("Bearer " <> t.accessToken)
                  ]
      })

storageK :: String
storageK = "shopie:auth-storage"
