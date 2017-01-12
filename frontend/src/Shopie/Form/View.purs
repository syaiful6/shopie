module Shopie.Form.View
  ( View(..)
  , mkView
  , getForm
  , postForm
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))
import Data.Identity (Identity)
import Data.Tuple (Tuple(Tuple))
import Data.Profunctor.Strong (second)
import Data.Validation.Semigroup(unV)

import Shopie.Form.Types (Env, FormInput, Path, Method(..))
import Shopie.Form.Internal.Encoding (FormEncType, formTreeEncType)
import Shopie.Form.Internal.Form (FormTree, Form, eval, toFormTree)


newtype View v = View
  { viewName    :: String
  , viewContext :: Path
  , viewInput   :: List (Tuple Path FormInput)
  , viewErrors  :: List (Tuple Path v)
  , viewMethod  :: Method
  }

instance functorView :: Functor View where
  map f (View r) =
    View $ r { viewErrors = map (second f) r.viewErrors }

mkView
  :: forall v. String -> Path -> List (Tuple Path FormInput)
  -> List (Tuple Path v) -> Method -> View v
mkView name ctx inp err method =
  View
    ({ viewName: name
     , viewContext: ctx
     , viewErrors: err
     , viewInput: inp
     , viewMethod: method
    })

instance showView :: Show v => Show (View v) where
  show (View r) =
    "View " <> intercalate " "
      ([ show r.viewName
      ,  show r.viewContext
      ,  show r.viewInput
      ,  show r.viewErrors
      ,  show r.viewMethod
      ])

getForm
  :: forall v m a. Monad m
  => String -> Form v m a -> m (Tuple (FormTree Identity v m a) (View v))
getForm name form = do
  form' <- toFormTree form
  pure $ Tuple form' $ mkView name Nil Nil Nil Get

postForm
  :: forall v m a. Monad m
  => String -> Form v m a -> (FormEncType -> m (Env m))
  -> m (Tuple (Tuple (FormTree Identity v m a) (View v)) (Maybe a))
postForm name form makeEnv = do
  form' <- toFormTree form
  env <- makeEnv $ formTreeEncType form'
  let env' = env <<< ((:) name)
  eval Post env' form' >>= \(Tuple r inp) -> pure $ case unV Left Right r of
    Left err -> Tuple (Tuple form' $ mkView name Nil inp err Get) Nothing
    Right x -> Tuple (Tuple form' $ mkView name Nil inp Nil Post) (Just x)
