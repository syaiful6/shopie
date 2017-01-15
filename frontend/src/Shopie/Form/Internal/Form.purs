module Shopie.Form.Internal.Form
  ( Form
  , Form'
  , FormTree
  , SomeForm
  , FormMeta(..)
  , pushRef, (.:)
  , getRef
  , monadic
  , field
  , formList
  , formMapV
  , transform
  , someForm
  , runSomeForm
  , forOptional
  , eval
  , eval'
  , toField
  , toFormTree
  , children
  , getMetadata
  , lookupForm
  , lookupFormMetadata
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Biapply (biapply)

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Day (Day, day, runDay)
import Data.Foldable (intercalate)
import Data.Identity (Identity(..))
import Data.Leibniz (type (~), coerceSymm)
import Data.List (List(Nil), (:), drop, concat, unzip, null)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((***), (&&&), second)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Validation.Semigroup(V, unV, invalid)

import Shopie.Form.Internal.Field (Field, SomeField, evalField, someField,
  fieldMapV, singleton)
import Shopie.Form.Types (Env, FormInput, Method, Path)


-- | Base type for a form.
type Form v m a = FormTree m v m a

--
type Form' v m a = FormTree Identity v m a

data FormTree t v m a
  = Ref String (FormTree t v m a)
  | Pure (Field v a)
  | Ap (Day (FormTree t v m) (FormTree t v m) a)
  | Map (Exists (MapF t v m a))
  | Monadic (t (FormTree t v m a))
  | FormList (Exists (FormListF t v m a))
  | Metadata (List FormMeta) (FormTree t v m a)

-- | Used to encode RankNTypes on Ap and Map constructor of FormTree
data MapF t v m a b = MapF (b -> m (V v a)) (FormTree t v m b)
data FormListF t v m a b
  = FormListF (DefaultList (FormTree t v m b)) (FormTree t v m (List Int)) (a ~ List b)

instance functorFormTree :: (Monad m, Semigroup v) => Functor (FormTree t v m) where
  map f (Ap d) = Ap (f <$> d)
  map f fa = transform (\x -> pure (pure (f x))) fa

instance applyFormTree :: (Monad m, Semigroup v) => Apply (FormTree t v m) where
  apply fa fb = apFT fa fb

instance applicativeFormTree :: (Monad m, Semigroup v) => Applicative (FormTree t v m) where
  pure = Pure <<< singleton

instance showFormTree :: Show (FormTree Identity v m a) where
  show = intercalate "\n" <<< showForm

-- -- | Value-agnostic Form
data SomeForm v m = SomeForm (Exists (SomeFormF v m))

data SomeFormF v m a = SomeFormF (Form' v m a)

instance showSomeForm :: Show (SomeForm v m) where
  show = runSomeForm show

-- | smart constructor for SomeForm
someForm :: forall v m a. Form' v m a -> SomeForm v m
someForm ft = SomeForm (mkExists (SomeFormF ft))

runSomeForm :: forall v m r. (forall a. Form' v m a -> r) -> SomeForm v m -> r
runSomeForm f (SomeForm d) = runExists (\(SomeFormF form) -> f form) d

--
data FormMeta = Disabled

derive instance eqMetadata :: Eq FormMeta

instance showMetadata :: Show FormMeta where
  show Disabled = "Disabled"

-- helper
mapFT :: forall t v m a b. (b -> m (V v a)) -> FormTree t v m b -> FormTree t v m a
mapFT f x = Map (mkExists (MapF f x))

apFT :: forall t v m a b. FormTree t v m (b -> a) -> FormTree t v m b -> FormTree t v m a
apFT f x = Ap (day ($) f x)

-- Helper for the FormTree Show instance
showForm :: forall v m a. Form' v m a -> List String
showForm form = case form of
  Ref r x ->
    ("Ref " <> show r) : (map indent (showForm x))
  Pure x ->
    ("Pure (" <> show x <> ")") : Nil
  Ap d ->
    runDay (\_ x y ->
      concat
        (("App" : Nil) : (map indent (showForm x)) : (map indent (showForm y)) : Nil)) d
  Map d ->
    runExists (\(MapF _ x) ->
      "Map _" : (map indent (showForm x))) d
  Monadic x ->
    "Monadic" : (map indent (showForm $ unwrap x))
  FormList d ->
    runExists (\(FormListF _ xs _) ->
      concat
        (("FormList <default>" : Nil) : map indent (showForm xs) : Nil )) d
  Metadata m x ->
    ("Metadata " <> show m) : map indent (showForm x)
  where
    indent = append " "

-- | Smart constructor to create FormList
formList
  :: forall t v m a.
  DefaultList (FormTree t v m a) -> FormTree t v m (List Int) -> FormTree t v m (List a)
formList def form = FormList (mkExists (FormListF def form id))

-- | Map on the value type
transform
  :: forall t v m a b. Monad m
  => (b -> m (V v a)) -> FormTree t v m b -> FormTree t v m a
transform f (Map mf) = runExists (\(MapF g x) -> mapFT (\y -> g y `bindV` f) x) mf
transform f x = mapFT f x

-- | Hide a monadic wrapper
monadic :: forall v m a. m (Form v m a) -> Form v m a
monadic = Monadic

-- | Build a field form the form
field :: forall t v m a . Field v a -> FormTree t v m a
field = Pure

bindV :: forall v m a b. Monad m => m (V v a) -> (a -> m (V v b)) -> m (V v b)
bindV m f = m >>= unV (pure <<< invalid) f

-- | Normalize a Form to allow operations on the contents
toFormTree :: forall v m a. Monad m => Form v m a -> m (Form' v m a)
toFormTree (Ref r x) = map (Ref r) (toFormTree x)
toFormTree (Pure x) = pure (Pure x)
toFormTree (Ap d) =
  runDay (\i x y -> day i <$> toFormTree x <*> toFormTree y <#> Ap) d
toFormTree (Map d) = runExists (\(MapF f x) -> map (mapFT f) (toFormTree x)) d
toFormTree (Monadic x) = x >>= toFormTree >>= pure <<< Monadic <<< Identity
toFormTree (FormList d) =
  runExists (\(FormListF d is proof) ->
    (\d' fis -> FormList (mkExists (FormListF d' fis proof)))
    <$> (traverse toFormTree d)
    <*> (toFormTree is)) d
toFormTree (Metadata m x) = map (Metadata m) (toFormTree x)

-- | Returns the topmost untransformed single field, if one exists
toField :: forall v m a. Form' v m a -> Maybe (SomeField v)
toField (Ref _ x)      = toField x
toField (Pure x)       = Just (someField x)
toField (Ap _)         = Nothing
toField (Map d)        = runExists (\(MapF _ x) -> toField x) d
toField (Monadic x)    = toField $ unwrap x
toField (FormList d)   = Nothing
toField (Metadata _ x) = toField x

-- | Returns the topmost applicative or index trees if either exists
-- otherwise returns an empty list
children :: forall v m a. Form' v m a -> List (SomeForm v m)
children (Ref _ x) = children x
children (Pure x) = Nil
children (Ap d) = runDay (\_ x y -> (someForm x : someForm y : Nil)) d
children (Map d) = runExists (\(MapF _ x) -> children x) d
children (Monadic x) = children $ unwrap x
children (FormList d) = runExists (\(FormListF _ is _) -> someForm is : Nil) d
children (Metadata _ x) = children x

-- | Operator to set a name for a subform.
pushRef :: forall t v m a. String -> FormTree t v m a -> FormTree t v m a
pushRef = Ref

infixr 5 pushRef as .:

-- Return topmost label of the tree if it exists, with the rest of the form
popRef :: forall v m a. Form' v m a -> Tuple (Maybe String) (Form' v m a)
popRef form = case form of
  Ref r x ->
    Tuple (Just r) x
  Pure _ ->
    Tuple Nothing form
  Ap _ ->
    Tuple Nothing form
  Map d ->
    runExists (\(MapF g x) -> second (mapFT g) (popRef x)) d
  Monadic x ->
    popRef $ unwrap x
  FormList d ->
    Tuple Nothing form
  Metadata m x -> second (Metadata m) (popRef x)

-- | Return the first/topmost label of a form
getRef :: forall v m a. Form' v m a -> Maybe String
getRef = fst <<< popRef

getMetadata :: forall v m a. Form' v m a -> List FormMeta
getMetadata (Map d) = runExists (\(MapF _ x) -> getMetadata x) d
getMetadata (Monadic x) = getMetadata $ unwrap x
getMetadata (Metadata m x) = m <> getMetadata x
getMetadata _ = Nil

--------------------------------------------------------------------------------
-- | Retrieve the form(s) at the given path
lookupForm :: forall v m a. Path -> Form' v m a -> List (SomeForm v m)
lookupForm path = map fst <<< lookupFormMetadata path

--
lookupFormMetadata :: forall v m a. Path -> Form' v m a -> List (Tuple (SomeForm v m) (List FormMeta))
lookupFormMetadata path = go Nil path <<< someForm
  where
    go md path' =
      runSomeForm \form ->
        case path' of
          Nil ->
            (Tuple (someForm form) (getMetadata form <> md) : Nil)
          (r : rs) -> case popRef form of
            Tuple (Just r') stripped
              | r == r' && null rs -> (Tuple (someForm stripped) (getMetadata form <> md) : Nil)
              | r == r'            -> children form >>= go (getMetadata form <> md) rs
              | otherwise          -> Nil
            Tuple Nothing _        -> children form >>= go (getMetadata form <> md) (r : rs)

formMapV :: forall v w m a. Monad m => (v -> w) -> Form' v m a -> Form' w m a
formMapV f (Ref r x) = Ref r $ formMapV f x
formMapV f (Pure x)  = Pure (fieldMapV f x)
formMapV f (Ap d)    = runDay (\i x y -> Ap $ day i (formMapV f x) (formMapV f y)) d
formMapV f (Map d)   = runExists (\(MapF g x) -> mapFT (g >=> pure <<< lmap f) (formMapV f x)) d
formMapV f (Monadic x) = formMapV f $ unwrap x
formMapV f (FormList x) =
  runExists (\(FormListF d is p) ->
    FormList (mkExists (FormListF (map (formMapV f) d) (formMapV f is) p))) x
formMapV f (Metadata m x) = Metadata m $ formMapV f x

ann :: forall v a. Path -> V v a -> V (List (Tuple Path v)) a
ann path = unV (invalid <<< pure <<< Tuple path) pure

eval
  :: forall v m a. Monad m
  => Method -> Env m -> Form' v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval = eval' Nil

eval'
  :: forall v m a. Monad m
  => Path -> Method -> Env m -> Form' v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval' p met env = case _ of
  Ref r x -> eval' (p <> (r : Nil)) met env x

  Pure fi ->
    (pure <<< flip (evalField met) fi &&& (_ >>= pure <<< Tuple p)) <$> env p

  Ap d ->
    runDay (\i x y ->
      biapply <<< (lift2 i *** append) <$> eval' p met env x <*> eval' p met env y) d

  Map d ->
    runExists (\(MapF f x) -> do
      Tuple x' inp <- eval' p met env x
      x_ <- bindV (pure x') (f >=> pure <<< ann p)
      pure (Tuple x_ inp)) d

  Monadic x ->
    eval' p met env $ unwrap x

  FormList d ->
    runExists (\(FormListF defs fis proof) -> do
      Tuple ris inp1 <- eval' p met env fis
      let ris' = unV Left Right ris
      case ris' of
        Left err -> pure (Tuple (invalid err) inp1)
        Right is ->
          (map (coerceSymm proof) <<< sequence *** append inp1 <<< concat) <<< unzip
          <$> traverse
            (\i -> eval' (p <> ((show i) : Nil))
                            met env $ defs `defaultListIndex` i) is) d

  Metadata _ x ->
    eval' p met env x

forOptional :: forall v b a. Semigroup v => (a -> V v b) -> Maybe a -> V v (Maybe b)
forOptional f = maybe (pure Nothing) (unV invalid (pure <<< pure) <<< f)

type DefaultList a = NonEmpty List a

defaultListIndex :: forall a. DefaultList a -> Int -> a
defaultListIndex (x :| xs) i
  | i < 0     = x
  | otherwise = case drop i xs of
      Nil -> x
      (y : _) -> y
