module Shopie.Form.Internal.Encoding where

import Prelude

import Data.Identity (Identity)
import Data.Exists (runExists)
import Data.Foldable (foldr)
import Data.List (List, (:), concatMap, mapMaybe)
import Data.Monoid (class Monoid, mempty)

import Shopie.Form.Internal.Field (Field(File), SomeField(..), SomeFieldF(..))
import Shopie.Form.Internal.Form (FormTree, SomeForm(..), SomeFormF(..), someForm, toField, children)


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
fieldEncType (File _) = MultiPart
fieldEncType _ = UrlEncoded

fieldList :: forall v m a. FormTree Identity v m a -> List (SomeField v)
fieldList = mapMaybe toField' <<< fieldList' <<< someForm
  where
    fieldList' (SomeForm d) =
      runExists (\(SomeFormF fld) -> someForm fld : concatMap fieldList' (children fld)) d
    toField' (SomeForm d) =
      runExists (\(SomeFormF fld) -> toField fld) d

formTreeEncType :: forall v m a. FormTree Identity v m a -> FormEncType
formTreeEncType = foldr append mempty <<< map fieldEncType' <<< fieldList
  where
    fieldEncType' (SomeField d) = runExists (\(SomeFieldF fld) -> fieldEncType fld) d
