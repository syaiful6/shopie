module Shopie.Auth.AuthF.Interpreter.Internal where

import Prelude

import Control.Applicative.Lift (Lift(Lifted))
import Control.Error.Util (hust, note)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free, liftF)

import Data.Argonaut (decodeJson, jsonParser, encodeJson, printJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.List as L
import Data.StrMap as SM

import Network.JsonApi as JA
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.StatusCode (StatusCode(..))

import Qyson.ConfigF as CF
import Qyson.Data (urlEncoded, DataQ)
import Qyson.QysonF.Interpreter.Internal (jsonResult)

import Shopie.Auth.StorageF as SF
import Shopie.Auth.Types (BearerToken, Oauth2Client(..), Creds(..), Email, userIdResult)


ask :: forall c r. Free (Coproduct (CF.ConfigF c) r) c
ask = liftF $ left $ CF.configF id

lift
  :: forall a b c f r
   . f r
  -> Free (Coproduct a (Coproduct b (Coproduct c (Lift f)))) r
litft = liftF <<< right <<< right <<< right <<< Lifted

bearerResult :: String -> Either Error BearerToken
bearerResult = jsonResult

userIdResult :: String -> Either Error UserId
userIdResult = jsonResult >=> resource >=> takeId
  where
    takeId :: forall e. JA.Identifier -> Either e UserId
    takeId (JA.Identifier {ident}) = Right ident

    resource :: JD.Document (SM.StrMap String) -> Either Error (JA.Identifier)
    resource =
      note (error "empty json api")
      <<< map JA.identifier
      <<< L.head
      <<< (_.resources)
      <<< JD.unDocument

persistBearerToken
  :: forall a b c
  -> String
   . BearerToken
  -> Free (Coproduct a (Coproduct b (Coproduct SF.StorageF c))) Unit
persistBearerToken k = persist k <<< printJson <<< encodeJson

restoreBearerToken
  :: forall a b c
   . String
   . Free (Coproduct a (Coproduct b (Coproduct SF.StorageF c))) (Maybe BearerToken)
restoreBearerToken s = (hust <<< (jsonParser >=> decodeJson) =<< _) <$> restore s

persist
  :: forall a b c
   . String
  -> String
  -> Free (Coproduct a (Coproduct b (Coproduct SF.StorageF c))) Unit
persist k v = liftF $ left $ right $ right $ SF.persist k v

restore
  :: forall a b c
   . String
  -> Free (Coproduct a (Coproduct b (Coproduct SF.StorageF c))) (Maybe String)
restore = liftF <<< left <<< right <<< right <<< SF.restore

remove
  :: forall a b c
   . String
  -> Free (Coproduct a (Coproduct b (Coproduct SF.StorageF c))) Unit
remove = liftF <<< left <<< right <<< right <<< SF.remove

mkRequest
  :: forall a b c r
   . (String -> Either Error r)
  -> AXF.AffjaxF RequestContent String
  -> Free (Coproduct a (Coproduct (AXF.AffjaxFP RequestContent String) b)) (Either Error r)
mkRequest k req = map (handleResponse >=> k) <<< liftF <<< left <<< right

handleResponse :: AffjaxResponse String -> Either Error String
handleResponse { status: StatusCode code, response, headers }
  | code >= 200 && code < 300 = Right response
  | code == 404 = Left $ error "Not Found."
  | code == 403 = Left $ error "Forbidden"
  | code == 402 = Left $ error "Payment required"
  | code == 401 = Left $ error "Unauthorized"
  | otherwise = Left $ error ("unknown error, response returned with code " <> show code)

authenticateBody :: Oauth2Client -> Creds -> DataQ
authenticateBody (Oauth2Client client) (Creds creds) =
  urlEncoded $ [ Tuple "username" creds.email
               , Tuple "password" creds.passwords
               , Tuple "grant_type" (Just "password")
               , Tuple "client_id" (Just client.clientId)
               , Tuple "client_secret" (Just client.clientSecret)
               ]

normalizeExpiration :: BearerToken -> BearerToken
normalizeExpiration (BearerToken to) =
  BearerToken $ to { expiresIn = normalizeExp $ to.expiresIn }

foreign import normalizeExp :: Number -> Number
