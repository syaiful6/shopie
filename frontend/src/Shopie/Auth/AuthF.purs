module Shopie.Auth.AuthF where

import Prelude

import Data.Maybe (Maybe)

import Shopie.Auth.Types (AuthResult, Creds, Email, UserId, TokenId)


data AuthF a
  = Authenticate Creds (AuthResult -> a)
  | MaybeAuthId (Maybe UserId -> a)
  | Invalidate (AuthResult -> a)
  | GetAuthTokenId (Maybe TokenId -> a)
  | Forgotten Email a

instance functorAuthF :: Functor AuthF where
  map f = case _ of
    Authenticate c g -> Authenticate c (f <<< g)
    MaybeAuthId g -> MaybeAuthId (f <<< g)
    Invalidate g -> Invalidate (f <<< g)
    GetAuthTokenId g -> GetAuthTokenId (f <<< g)
    Forgotten e a -> Forgotten e (f a)

authenticateF :: Creds -> AuthF AuthResult
authenticateF = flip Authenticate id

maybeAuthIdF :: AuthF (Maybe UserId)
maybeAuthIdF = MaybeAuthId id

invalidateF :: AuthF AuthResult
invalidateF = Invalidate id

getAuthTokenId :: AuthF (Maybe TokenId)
getAuthTokenId = GetAuthTokenId id

forgottenF :: Email -> AuthF Unit
forgottenF = flip Forgotten unit
