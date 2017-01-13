module Shopie.Form.Internal.Encoding
  ( FormEncType(..)
  , fieldEncType
  , fieldList
  , formTreeEncType
  ) where

import Prelude

import Data.Foldable (foldr)
import Data.List (List, (:), concatMap, mapMaybe)
import Data.Monoid (class Monoid, mempty)

import Shopie.Form.Internal.Field (Field, SomeField, isFile, runSomeField)
import Shopie.Form.Internal.Form (Form', someForm, runSomeForm, toField, children)


data FormEncType = UrlEncoded | MultiPart

derive instance eqFormEncType :: Eq FormEncType

instance showFormEncType :: Show FormEncType where
  show UrlEncoded = "UrlEncoded"
  show MultiPart = "MultiPart"

instance semigroupFormEncType :: Semigroup FormEncType where
  append UrlEncoded x = x
  append MultiPart _ = MultiPart

instance monoidFormEncType :: Monoid FormEncType where
  mempty = UrlEncoded

fieldEncType :: forall v a. Field v a -> FormEncType
fieldEncType fi = if isFile fi then MultiPart else UrlEncoded

fieldList :: forall v m a. Form' v m a -> List (SomeField v)
fieldList = mapMaybe toField' <<< fieldList' <<< someForm
  where
    fieldList' = runSomeForm (\fi -> someForm fi : concatMap fieldList' (children fi))
    toField' = runSomeForm toField

formTreeEncType :: forall v m a. Form' v m a -> FormEncType
formTreeEncType = foldr append mempty <<< map (runSomeField fieldEncType) <<< fieldList
