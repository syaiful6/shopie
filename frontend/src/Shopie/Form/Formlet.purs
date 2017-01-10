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
  , validateM
  , module ExposeForm
  ) where

import Prelude

import Data.List (List(Nil), (:), head, findIndex, fromFoldable)
import Data.List.Lazy as LZ
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Validation.Semigroup(V, invalid)

import Shopie.Form.Internal.Field as IF
import Shopie.Form.Internal.Form as FO
import Shopie.Form.Internal.Form (Form, FormTree, (.:)) as ExposeForm
import Shopie.Form.Types (FilePath)


type Formlet v m a = Maybe a -> FO.Form v m a

text :: forall v m. (Monad m, Semigroup v) => Formlet v m String
text def = FO.Pure $ IF.text $ fromMaybe "" def

choice
  :: forall v m a
   . (Eq a, Monad m, Semigroup v)
  => NonEmpty List (Tuple a v)
  -> Formlet v m a
choice (def':|items) def =
  let
    pair s t = Tuple (show s) t
    zipped = pair <$> ZipList (LZ.iterate (add 1) 1) <*> ZipList (LZ.fromFoldable items)
  in choiceWith (pair 0 def' :| fromFoldable (unwrap zipped)) def

choiceWith
  :: forall v m a
   . (Eq a, Monad m, Semigroup v)
  => NonEmpty List (Tuple String (Tuple a v))
  -> Formlet v m a
choiceWith ne@(_:|items) def =
  choiceWith' ne (def >>= (\d -> findIndex ((eq d) <<< fst <<< snd) items))

choiceWith'
  :: forall v m a
   . (Monad m, Semigroup v)
  => NonEmpty List (Tuple String (Tuple a v))
  -> Maybe Int
  -> FO.Form v m a
choiceWith' (default':|items) def =
  fromMaybe defaultItem <<< head <<< map fst <$> (FO.Pure (IF.choice (Tuple "" merged : Nil) def'))
  where
    merged = (default' : items)
    defaultItem = fst $ snd $ default'
    def' = case def of
      Just x  -> (x : Nil)
      Nothing -> (0 : Nil)

bool :: forall v m. (Monad m, Semigroup v) => Formlet v m Boolean
bool = FO.Pure <<< IF.bool <<< fromMaybe false

file :: forall v m. (Monad m, Semigroup v) => FO.Form v m (Maybe FilePath)
file = head <$> FO.Pure IF.file

check
  :: forall v m a
   . (Monad m, Semigroup v)
  => v
  -> (a -> Boolean)
  -> FO.Form v m a
  -> FO.Form v m a
check err = checkM err <<< compose pure

checkM
  :: forall v m a
   . (Monad m, Semigroup v)
  => v
  -> (a -> m Boolean)
  -> FO.Form v m a
  -> FO.Form v m a
checkM err predicate form = validateM f form
  where
    f x = do
      r <- predicate x
      pure $ if r then pure x else invalid err

validateM
  :: forall v m a b
   . Monad m
  => (a -> m (V v b))
  -> FO.Form v m a
  -> FO.Form v m b
validateM = FO.transform
