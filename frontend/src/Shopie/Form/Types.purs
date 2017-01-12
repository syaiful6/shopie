module Shopie.Form.Types where

import Prelude

import Data.Foldable (intercalate)
import Data.List (List, filter, fromFoldable)
import Data.NonEmpty (NonEmpty)
import Data.String as S


data Method = Get | Post

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Eq Method
instance showMethod :: Show Method where
  show Get = "Get"
  show Post = "Post"

type Path = List String

toPath :: String -> Path
toPath = filter (not <<< S.null) <<< fromFoldable <<< S.split (S.Pattern ".")

fromPath :: Path -> String
fromPath = intercalate "."

type FilePath = String

data FormInput = TextInput String | FileInput String

derive instance eqFormInput :: Eq FormInput
derive instance ordFormInput :: Eq FormInput
instance showFormInput :: Show FormInput where
  show (TextInput t) = "(TextInput " <> t <> ")"
  show (FileInput t) = "(FileInput " <> t <> ")"

type Env m = Path -> m (List FormInput)

-- | NonEmpty List
type Nel a = NonEmpty List a
