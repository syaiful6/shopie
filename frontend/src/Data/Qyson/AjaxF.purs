module Data.Qyson.AffjaxF where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error, error)

import Data.Either (Either(..))
import Data.Functor.Pairing (Pairing)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))

import Network.HTTP.Affjax (URL, AffjaxRequest, AffjaxResponse, defaultRequest)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)

import Data.Qyson.Utils (pairArrowTuple)


data AffjaxF req res a =
  AffjaxF
    (AffjaxRequest req)
    (Either Error (AffjaxResponse res) -> a)

type AffjaxFE req res = AffjaxF req res (Either Error (AffjaxResponse res))

-- | Makes an `Affjax` request.
affjax :: forall a b. (Requestable a, Respondable b) => AffjaxRequest a -> AffjaxFE a b
affjax req = AffjaxF req id

-- | Makes a `GET` request to the specified URL.
get :: forall a. (Respondable a) => URL -> AffjaxFE Unit a
get u = affjax (defaultRequest { url = u, content = Nothing })

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall a b. (Requestable a, Respondable b) => URL -> a -> AffjaxFE a b
post u c = affjax (defaultRequest { method = Left POST, url = u, content = Just c })

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall a b. (Requestable a, Respondable b) => URL -> Maybe a -> AffjaxFE a b
post' u c = affjax (defaultRequest { method = Left POST, url = u, content = c })

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: forall a. (Requestable a) => URL -> a -> AffjaxFE a Unit
post_ = post

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: forall a. (Requestable a) => URL -> Maybe a -> AffjaxFE a Unit
post_' = post'

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall a b. (Requestable a, Respondable b) => URL -> a -> AffjaxFE a b
put u c = affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall a b. (Requestable a, Respondable b) => URL -> Maybe a -> AffjaxFE a b
put' u c = affjax (defaultRequest { method = Left PUT, url = u, content = c })

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: forall a. (Requestable a) => URL -> a -> AffjaxFE a Unit
put_ = put

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: forall a. (Requestable a) => URL -> Maybe a -> AffjaxFE a Unit
put_' = put'

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. (Respondable a) => URL -> AffjaxFE Unit a
delete u = affjax (defaultRequest { method = Left DELETE, url = u })

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: URL -> AffjaxFE Unit Unit
delete_ = delete

instance functorAffjaxFP :: Functor (AffjaxF req res) where
  map f (AffjaxF req g) = AffjaxF req (f <<< g)

data CoaffjaxF req res a =
  CoaffjaxF (AffjaxRequest req -> Tuple (Either Error (AffjaxResponse res)) a)

type CoaffjaxFE req res = CoaffjaxF req res (Either Error (AffjaxResponse res))

instance functorCoaffjaxF :: Functor (CoaffjaxF req res) where
  map f (CoaffjaxF k) = CoaffjaxF (map (map f) k)

pairAffjaxF :: forall req res. Pairing (AffjaxF req res) (CoaffjaxF req res)
pairAffjaxF f (AffjaxF req k) (CoaffjaxF h) = pairArrowTuple f k (h req)

coAffjax'
  :: forall eff req res
   . (Requestable req, Respondable res)
  => AffjaxRequest req
  -> (Tuple
        (Either Error (AffjaxResponse res))
        (Aff (ajax :: AX.AJAX | eff) (Either Error (AffjaxResponse res)))
      )
coAffjax' req = Tuple (Left (error "Init")) (attempt (AX.affjax req))
