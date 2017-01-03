module Shopie.Auth.Interpreter.Internal where

import Prelude

import Control.Monad.Aff.Free (class Affable, fromEff, fromAff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Applicative.Lift (Lift(Lifted))
import Control.Monad.Free (Free, liftF)

import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, left, right)

import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF

import Qyson.ConfigF as CF

type AXFP = AXF.AffjaxFP RequestContent String

type M g m = Free (Coproduct (CF.ConfigF Wiring g) (Coproduct (Lift m) AXFP))

ask :: forall c r. (Coproduct (CF.ConfigF c) r) c
ask = liftF $ left $ CF.configF id

lift :: forall g m a. m a -> M g m a
lift ma = liftF $ right $ left $ Lifted ma

liftEff :: forall g m a. AF.Affable m => m a -> M g m a
liftEff = lift <<< fromEff

liftAff :: forall g m a. AF.Affable m => m a -> M g m a
liftAff = lift <<< fromAff

liftAXF :: forall g m a. AXFP a -> M g m a
liftAXF = liftF <<< right <<< right

mkRequest :: forall g m a. (String -> Either Error a) -> AXFP a -> M g m a
mkRequest k req = map (handleResponse k) <<< liftAXF

handleResponse
  :: forall a
   . (String -> Either Error a)
  -> Either Error (AffjaxResponse String)
  -> Either Error a
handleResponse k = bind (go >=> k)
  where
    go :: AffjaxResponse String -> Either Error String
    go { status: StatusCode code, response, headers }
      | code >= 200 && code < 300 = Right response
