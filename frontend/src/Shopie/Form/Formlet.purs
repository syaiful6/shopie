module Shopie.Form.Formlet
  ( Formlet
  , text
  , choice
  , choiceWith
  , choiceWith'
  , bool
  , file
  , check
  , checkM
  , validate
  , validateOptional
  , validateM
  , module ExposeForm
  , module ExposeEnc
  ) where

import Prelude

import Data.List (List(Nil), (:), catMaybes, head, findIndex, fromFoldable)
import Data.List.Lazy as LZ
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Validation.Semigroup(V, invalid)

import Shopie.Form.Internal.Field as IF
import Shopie.Form.Internal.Form as FO
import Shopie.Form.Internal.Form (Form, Form', FormTree, (.:), monadic) as ExposeForm
import Shopie.Form.Internal.Encoding (FormEncType(..)) as ExposeEnc
import Shopie.Form.Types (FilePath, Nel)


type Formlet v m a = Maybe a -> FO.Form v m a

text :: forall v m. (Monad m, Semigroup v) => Formlet v m String
text def = FO.field $ IF.text $ fromMaybe "" def

choice
  :: forall v m a. (Eq a, Monad m, Semigroup v)
  => Nel (Tuple a v) -> Formlet v m a
choice (def':|items) def =
  let
    zipped = Tuple <$> ZipList (makeRefs 1) <*> ZipList (LZ.fromFoldable items)
  in choiceWith (Tuple "0" def' :| fromFoldable (unwrap zipped)) def

choiceWith
  :: forall v m a. (Eq a, Monad m, Semigroup v)
  => Nel (Tuple String (Tuple a v)) -> Formlet v m a
choiceWith ne@(_:|items) def =
  choiceWith' ne (def >>= (\d -> findIndex ((eq d) <<< fst <<< snd) items))

choiceWith'
  :: forall v m a. (Monad m, Semigroup v)
  => Nel (Tuple String (Tuple a v)) -> Maybe Int -> FO.Form v m a
choiceWith' (default':|items) def =
  fromMaybe defaultItem <<< head <<< map fst <$> (FO.field (IF.choice (Tuple "" merged : Nil) def'))
  where
    merged = (default' : items)
    defaultItem = fst $ snd $ default'
    def' = case def of
      Just x  -> (x : Nil)
      Nothing -> (0 : Nil)

choiceMultiple
  :: forall v m a. (Eq a, Monad m, Semigroup v)
  => List (Tuple a v) -> Formlet v m (List a)
choiceMultiple items def =
  let
    zipped = Tuple <$> ZipList (makeRefs 1) <*> ZipList (LZ.fromFoldable items)
  in choiceWithMultiple (fromFoldable (unwrap zipped)) def

choiceWithMultiple
  :: forall v m a. (Eq a, Monad m, Semigroup v)
  => List (Tuple String (Tuple a v)) -> Formlet v m (List a)
choiceWithMultiple items def = choiceWithMultiple' items def'
  where
    def' = def >>= Just <<< catMaybes <<< map (\d -> findIndex ((eq d) <<< fst <<< snd) items)

choiceWithMultiple'
  :: forall v m a. (Monad m, Semigroup v)
  => List (Tuple String (Tuple a v)) -> Maybe (List Int) -> FO.Form v m (List a)
choiceWithMultiple' items def = map fst <$> (FO.field $ IF.choice ((Tuple "" items : Nil)) def')
  where
    def' = case def of
      Just x -> x
      Nothing -> Nil

bool :: forall v m. (Monad m, Semigroup v) => Formlet v m Boolean
bool = FO.field <<< IF.bool <<< fromMaybe false

file :: forall v m. (Monad m, Semigroup v) => FO.Form v m (Maybe FilePath)
file = head <$> FO.field IF.file

check
  :: forall v m a. (Monad m, Semigroup v)
  => v -> (a -> Boolean) -> FO.Form v m a -> FO.Form v m a
check err = checkM err <<< compose pure

checkM
  :: forall v m a. (Monad m, Semigroup v)
  => v -> (a -> m Boolean) -> FO.Form v m a -> FO.Form v m a
checkM err predicate form = validateM f form
  where
    f x = do
      r <- predicate x
      pure $ if r then pure x else invalid err

validate :: forall v m a b. Monad m => (a -> V v b) -> FO.Form v m a -> FO.Form v m b
validate = validateM <<< compose pure

validateOptional
  :: forall v m a b. (Monad m, Semigroup v)
  => (a -> V v b) -> FO.Form v m (Maybe a) -> FO.Form v m (Maybe b)
validateOptional f = validate (FO.forOptional f)

validateM
  :: forall v m a b. Monad m
  => (a -> m (V v b)) -> FO.Form v m a -> FO.Form v m b
validateM = FO.transform

makeRefs :: Int -> LZ.List String
makeRefs s = show <$> LZ.iterate (add s) s
