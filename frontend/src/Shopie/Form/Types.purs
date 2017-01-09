module Shopie.Form.Types where

import Prelude

import Data.Foldable (intercalate)
import Data.List (List, filter, fromFoldable)
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

data FormInput = TextInput String | FileInput String

type Env m = Path -> m (List FormInput)
