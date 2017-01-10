module Shopie.Form.Internal.Field
  ( Field(..)
  , SomeField(..)
  , Grouped
  , ChoiceF(..)
  , SomeFieldF(..)
  , SomeField(..)
  , someField
  , singleton
  , text
  , bool
  , choice
  , file
  , fieldMapV
  , evalField
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Leibniz (type (~), coerceSymm)
import Data.List (List(Nil), (:), (!!), catMaybes, head, reverse, concat, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(Tuple), snd, fst)

import Shopie.Form.Types (Method(..), FilePath, FormInput(..), toPath)

-- [(Text, [(Text, (a, v))])]
type Grouped v b = List (Tuple String (List (Tuple String (Tuple b v))))
data ChoiceF v a b = ChoiceF (Grouped v b) (List Int) (a ~ (List (Tuple b Int)))

data Field v a
  = Text String (a ~ String)
  | Bool Boolean (a ~ Boolean)
  | Choice (Exists (ChoiceF v a))
  | Singleton a
  | File (a ~ (List FilePath))

data SomeFieldF v a = SomeFieldF (Field v a)

data SomeField v = SomeField (Exists (SomeFieldF v))

someField :: forall v a. Field v a -> SomeField v
someField f = SomeField (mkExists (SomeFieldF f))

instance showField :: Show (Field v a) where
  show (Text s _) = "Text " <> show s
  show (Bool b _) = "Bool " <> show b
  show (Choice _) = "Choice"
  show (Singleton _) = "Singleton"
  show (File _) = "File"

singleton :: forall v a. a -> Field v a
singleton a = Singleton a

text :: forall v. String -> Field v String
text s = Text s id

bool :: forall v. Boolean -> Field v Boolean
bool b = Bool b id

choice :: forall v a. Grouped v a -> (List Int) -> Field v (List (Tuple a Int))
choice g i = Choice (mkExists (ChoiceF g i id))

file :: forall v a. Field v (List FilePath)
file = File id

fieldMapV :: forall v w a. (v -> w) -> Field v a -> Field w a
fieldMapV _ (Singleton x) = Singleton x
fieldMapV _ (Text x proof) = Text x proof
fieldMapV _ (Bool x proof) = Bool x proof
fieldMapV f (Choice d) =
  runExists (\(ChoiceF xs i p) ->
    Choice (mkExists (ChoiceF (map (second (map (second (second f)))) xs) i p))) d
fieldMapV _ (File p) = File p

evalField
  :: forall v a
   . Method
  -> List FormInput
  -> Field v a
  -> a
evalField _ _ (Singleton x) = x
evalField _ (TextInput x : _) (Text _ proof) = coerceSymm proof x
evalField _ _ (Text x proof) = coerceSymm proof x
evalField Get _ (Choice d) =
  runExists (\(ChoiceF ls x proof) ->
    let ls' = concat (map snd ls) in
    coerceSymm proof $ catMaybes $ (map (\i -> flip Tuple i <<< fst <<< snd <$> ls' !! i) x)
  ) d
evalField Post _ (Choice d) =
  runExists (\(ChoiceF _ _ proof) -> coerceSymm proof Nil) d
evalField _ ts@(TextInput _ : _) (Choice d) =
  runExists (\(ChoiceF xs i proof) ->
    let ls' = concat (map snd xs) in
    coerceSymm proof $ catMaybes $
      map (\x' ->
        case x' of
          TextInput x -> do
            t <- head <<< reverse $ toPath x
            Tuple c i <- lookupIdx t ls'
            pure (Tuple (fst c) i)
          FileInput _ -> Nothing
      ) ts
  ) d
evalField Get _ (Bool x proof) = coerceSymm proof x
evalField Post (TextInput x : _) (Bool _ proof) = coerceSymm proof (x == "on")
evalField Post _ (Bool _ proof) = coerceSymm proof false
evalField Post xs (File proof) =
  let
    maybeFile (FileInput x) = Just x
    maybeFile _ = Nothing
  in coerceSymm proof $ mapMaybe maybeFile xs
evalField _ _ (File proof) = coerceSymm proof Nil

lookupIdx :: forall k v. Eq k => k -> List (Tuple k v) -> Maybe (Tuple v Int)
lookupIdx key = go 0
  where
    go _ Nil = Nothing
    go i ((Tuple k v) : xs)
      | key == k = Just (Tuple v i)
      | otherwise = go (i + 1) xs
