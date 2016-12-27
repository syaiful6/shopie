module Data.Qyson.Error where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Control.Monad.Eff.Exception (Error, error, message)


newtype UnauthorizedMessage = UnauthorizedMessage String

data ErrorQ
  = NotFound
  | Unauthorized (Maybe UnauthorizedMessage)
  | Forbidden
  | PaymentRequired
  | Error Error

instance showErrorQ :: Show ErrorQ where
  show NotFound = "NotFound"
  show (Unauthorized Nothing) = "Unauthorized"
  show (Unauthorized (Just (UnauthorizedMessage msg))) = "Unauthorized: " <> msg
  show Forbidden = "Forbidden"
  show PaymentRequired = "PaymentRequired"
  show (Error err) = "Unknown " <> show err

printErrorQ :: ErrorQ -> String
printErrorQ = case _ of
  NotFound -> "Resource not found"
  Unauthorized _ -> "Resource is unavailable, authorization is required"
  Forbidden -> "Resource is unavailable, the current authorization credentials do not grant access to the resource"
  PaymentRequired -> "Resource is unavailable, payment is required to use this feature"
  Error err -> message err

lowerErrorQ :: ErrorQ → Error
lowerErrorQ = case _ of
  Error err -> err
  qe -> error (printErrorQ qe)

type ResponseQ resp = Either ErrorQ resp
type ContinuationQ resp next = ResponseQ resp -> next

infixr 2 type ContinuationQ as !~>
