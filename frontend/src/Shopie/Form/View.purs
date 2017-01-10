module Shopie.Form.View
  ( ViewR(..)
  , View
  , mkView
  , unView
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
import Shopie.Form.Internal.Form (FormTree, Form, eval, formMapV, toFormTree)

import Unsafe.Coerce (unsafeCoerce)

data ViewR v (m :: * -> *) a = ViewR
  { viewName    :: String
  , viewContext :: Path
  , viewForm    :: FormTree Identity v m a
  , viewInput   :: List (Tuple Path FormInput)
  , viewErrors  :: List (Tuple Path v)
  , viewMethod  :: Method
  }

data View v

instance functorView :: Functor View where
  map f v =
    unView (\(ViewR r@{ viewForm, viewErrors }) ->
      mkView
        r.viewName
        r.viewContext
        (formMapV f viewForm)
        r.viewInput
        (map (second f) viewErrors)
        r.viewMethod
    ) v

mkView
  :: forall v m a
  .  Monad m
  => String
  -> Path
  -> FormTree Identity v m a
  -> List (Tuple Path FormInput)
  -> List (Tuple Path v)
  -> Method
  -> View v
mkView name ctx form inp err method =
  view $ ViewR
    ({ viewName: name
     , viewContext: ctx
     , viewForm: form
     , viewInput: inp
     , viewErrors: err
     , viewMethod: method
    })

view :: forall v m a. ViewR v m a -> View v
view = unsafeCoerce

unView :: forall v r. (forall m a. Monad m => ViewR v m a -> r) -> View v -> r
unView = unsafeCoerce

instance showView :: Show v => Show (View v) where
  show v =
    unView (\(ViewR r) ->
      "View " <> intercalate " "
        ([ show r.viewName
        ,  show r.viewContext
        ,  show r.viewForm
        ,  show r.viewInput
        ,  show r.viewErrors
        ,  show r.viewMethod
        ])
    ) v

getForm :: forall v m a. Monad m => String -> Form v m a -> m (View v)
getForm name form = do
  form' <- toFormTree form
  pure $ mkView name Nil form' Nil Nil Get

postForm
  :: forall v m a
   . Monad m
  => String
  -> Form v m a
  -> (FormEncType -> m (Env m))
  -> m (Tuple (View v) (Maybe a))
postForm name form makeEnv = do
  form' <- toFormTree form
  env <- makeEnv $ formTreeEncType form'
  let env' = env <<< ((:) name)
  eval Post env' form' >>= \(Tuple r inp) -> pure $ case unV Left Right r of
    Left err -> Tuple (mkView name Nil form' inp err Get) Nothing
    Right x -> Tuple (mkView name Nil form' inp Nil Post) (Just x)
