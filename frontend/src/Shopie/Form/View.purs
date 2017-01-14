module Shopie.Form.View
  ( View(..)
  , mkView
  , postForm
  , runFormPure
  , viewStrError
  , module ExposeIntern
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:), filter)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Profunctor.Strong (first, second)
import Data.Validation.Semigroup(unV)

import Shopie.Form.Types (Env, FormInput(TextInput), Path, Method(..), fromPath)
import Shopie.Form.Internal.Encoding (FormEncType, formTreeEncType)
import Shopie.Form.Internal.Form (toFormTree) as ExposeIntern
import Shopie.Form.Internal.Form (Form, eval, toFormTree)


newtype View v = View
  { viewName    :: String
  , viewContext :: Path
  , viewInput   :: List (Tuple Path FormInput)
  , viewErrors  :: List (Tuple Path v)
  , viewMethod  :: Method
  }

instance functorView :: Functor View where
  map f (View r@{ viewErrors }) =
    View $ r { viewErrors = map (second f) viewErrors }

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

postForm
  :: forall v m a. Monad m
  => String -> Form v m a -> (FormEncType -> m (Env m))
  -> m (Tuple (View v) (Maybe a))
postForm name form makeEnv = do
  form' <- toFormTree form
  env <- makeEnv $ formTreeEncType form'
  let env' = env <<< ((:) name)
  eval Post env' form' >>= \(Tuple r inp) -> pure $ case unV Left Right r of
    Left err -> Tuple (mkView name Nil inp err Get) Nothing
    Right x -> Tuple (mkView name Nil inp Nil Post) (Just x)

runFormPure
  :: forall v m a. Monad m
  => String -- form name
  -> Form v m a -- form
  -> List (Tuple String String) -- the static env
  -> m (Tuple (View v) (Maybe a))
runFormPure name form input = postForm name form \_ -> pure $ \key ->
  pure $ map (TextInput <<< snd) $ filter (eq (fromPath key) <<< fst) input

viewStrError :: forall v. View v -> List (Tuple String v)
viewStrError (View { viewErrors }) = first fromPath <$> viewErrors
