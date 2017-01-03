module Shopie.Auth.Class where

import Shopie.Prelude

import Control.Monad.Free (Free, liftF)

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

import Shopie.Auth.Types (UserId, AuthResult)


class AuthDSL m where
  -- | authenticate a given credentials
  authenticate :: Creds -> m AuthMessage
  -- | Retrieves user credentials, if user is authenticated.
  maybeAuthId :: m (Maybe UserId)
  -- | invalidate the current user. Should be okay calling this multiple times.
  invalidate :: m AuthResult

instance authDSLMaybeT :: (Monad m, AuthDSL m) => AuthDSL (MaybeT m) where
  authenticate = lift <<< authenticate
  maybeAuthId  = lift $ maybeAuthId
  invalidate   = lift $ invalidate

instance authDSLExceptT :: (Monad m, AuthDSL m) => AuthDSL (ExceptT e m) where
  authenticate = lift <<< authenticate
  maybeAuthId  = lift $ maybeAuthId
  invalidate   = lift $ invalidate

instance authDSLFree :: AuthDSL f => AuthDSL (Free f) where
  authenticate = liftF <<< authenticate
  maybeAuthId  = liftF $ maybeAuthId
  invalidate   = liftF $ invalidate

instance authDSLHalogenF :: AuthDSL g => AuthDSL (HF.HalogenFP ES.EventSource s f g) where
  authenticate = HF.QueryHF <<< authenticate
  maybeAuthId  = HF.QueryHF $ maybeAuthId
  invalidate   = HF.QueryHF $ invalidate

instance authDSLHalogenFP :: AuthDSL g => AuthDSL (
                                            HF.HalogenFP ES.ParentEventSource s f
                                              (Free
                                                (HF.HalogenFP ES.EventSource s' f' g)
                                              )
                                            ) where
  authenticate = HF.QueryHF <<< authenticate
  maybeAuthId  = HF.QueryHF $ maybeAuthId
  invalidate   = HF.QueryHF $ invalidate
