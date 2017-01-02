module Qyson.QysonF.Interpreter.Internal where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free, liftF)

import Data.Argonaut ((.?))
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.List (List(..), (:))
import Data.List as List
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (Sandboxed, Rel, Path, Abs, RelDir, file, dir, printPath,
                        rootDir, relativeTo, (</>))
import Data.String as Str
import Data.Tuple (Tuple(..))

import Global (encodeURIComponent)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.ResponseHeader as RH

import Qyson.ConfigF as CF
import Qyson.QysonF (ErrorQ(..), UnauthorizedMessage(..), AnyPath, Pagination(..))


type AXFP = AXF.AffjaxFP RequestContent String

defaultRequest :: AX.AffjaxRequest RequestContent
defaultRequest = AX.defaultRequest { content = Nothing }

ask :: forall c r. Free (Coproduct (CF.ConfigF c) r) c
ask = liftF $ left $ CF.configF id

jsonResult :: forall j. Json.DecodeJson j => String -> Either Error j
jsonResult = lmap error <$> (Json.decodeJson <=< Json.jsonParser)

strResult :: String -> Either Error String
strResult = Right

unitResult :: String -> Either Error Unit
unitResult = const (Right unit)

toPageParams :: Maybe Pagination -> List (Tuple String String)
toPageParams Nothing = Nil
toPageParams (Just (Pagination offset limit)) =
  ( Tuple "offset" (show offset)
  : Tuple "limit" (show limit)
  : Nil
  )

getReq :: AX.URL -> AXF.AffjaxF RequestContent String
getReq u = AXF.affjax (defaultRequest { url = u })

putReq :: AX.URL -> RequestContent -> AXF.AffjaxF RequestContent String
putReq u c = AXF.affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

deleteReq :: AX.URL -> AXF.AffjaxF RequestContent String
deleteReq u = AXF.affjax (defaultRequest { method = Left DELETE, url = u })

mkURL
  :: forall r
   . RelDir Sandboxed
  -> AnyPath
  -> List (Tuple String String)
  -> Free (Coproduct (CF.ConfigF { basePath :: String | r }) AXFP) String
mkURL endpoint path params = do
  { basePath } <- ask
  let url = basePath <> mkPath endpoint path
  pure case params of
    Nil -> url
    _ -> url <> toQueryString params

toQueryString :: List (Tuple String String) -> String
toQueryString
  = ("?" <> _)
  <<< Str.joinWith "&"
  <<< List.toUnfoldable
  <<< map (\(Tuple k v) → k <> "=" <> encodeURIComponent v)

mkPath :: RelDir Sandboxed -> AnyPath -> String
mkPath base fsPath
  = Str.drop 1
  $ Str.joinWith "/"
  $ map encodeURIComponent
  $ Str.split (Str.Pattern "/")
  $ either printPath printPath
  $ bimap (baseify (dir "/")) (baseify (file "")) fsPath
  where
    baseify :: forall b. Path Rel b Sandboxed -> Path Abs b Sandboxed -> Path Rel b Sandboxed
    baseify x p = base </> fromMaybe x (relativeTo p rootDir)

mkRequest
  :: forall a l
   . (String -> Either Error a)
  -> AXF.AffjaxF RequestContent String
  -> Free (Coproduct l AXFP) (Either ErrorQ a)
mkRequest k = map (handleResult k) <<< liftF <<< right

handleResult
  :: forall a
   . (String -> Either Error a)
  -> Either Error (AX.AffjaxResponse String)
  -> Either ErrorQ a
handleResult k = case _ of
  Right { status: StatusCode code, response, headers }
      | code >= 200 && code < 300 -> lmap ExceptQ (k response)
      | code == 404 -> Left NotFound
      | code == 403 -> Left Forbidden
      | code == 402 -> Left PaymentRequired
      | code == 401 ->
          Left
            $ Unauthorized
            $ (UnauthorizedMessage <<< show)
            <$> (Array.index headers =<< Array.findIndex isWWWAuthenticate headers)
      | otherwise ->
          Left $ ExceptQ $ error $
            either (pure $ "An unknown error ocurred: " <> show code <> " " <> show response) id $
              (_ .? "error") =<< (Json.decodeJson =<< Json.jsonParser response)
  Left err -> Left (ExceptQ err)
  where
    isWWWAuthenticate :: RH.ResponseHeader -> Boolean
    isWWWAuthenticate = eq "www-authenticate" <<< Str.toLower <<< RH.responseHeaderName
