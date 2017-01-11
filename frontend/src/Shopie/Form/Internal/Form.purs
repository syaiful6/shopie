module Shopie.Form.Internal.Form where

import Prelude

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
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Validation.Semigroup(V, unV, invalid)

import Shopie.Form.Internal.Field ( Field(Singleton), SomeField, evalField, someField
                                  , fieldMapV)
import Shopie.Form.Types (Env, FormInput, Method, Path)


-- | Base type for a form.
type Form v m a = FormTree m v m a

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
  apply (Pure (Singleton k)) (Pure (Singleton a)) = Pure (Singleton (k a))
  apply (Pure (Singleton k)) (Ap d) = Ap (k <$> d)
  apply fa fb = apFT fa fb

instance applicativeFormTree :: (Monad m, Semigroup v) => Applicative (FormTree t v m) where
  pure = Pure <<< Singleton

instance showFormTree :: Show (FormTree Identity v m a) where
  show = intercalate "\n" <<< showForm

-- -- | Value-agnostic Form
data SomeForm v m = SomeForm (Exists (SomeFormF v m))

data SomeFormF v m a = SomeFormF (FormTree Identity v m a)

instance showSomeForm :: Show (SomeForm v m) where
  show (SomeForm d) = runExists (\(SomeFormF f) -> show f) d

-- | smart constructor for SomeForm
someForm :: forall v m a. FormTree Identity v m a -> SomeForm v m
someForm ft = SomeForm (mkExists (SomeFormF ft))

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
showForm :: forall v m a. FormTree Identity v m a -> List String
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

bindV :: forall v m a b. Monad m => m (V v a) -> (a -> m (V v b)) -> m (V v b)
bindV m f = m >>= unV (pure <<< invalid) f

-- | Normalize a Form to allow operations on the contents
toFormTree :: forall v m a. Monad m => Form v m a -> m (FormTree Identity v m a)
toFormTree (Ref r x) = map (Ref r) (toFormTree x)
toFormTree (Pure x) = pure (Pure x)
toFormTree (Ap d) =
  runDay (\i x y -> day i <$> (toFormTree x) <*> (toFormTree y) <#> Ap) d
toFormTree (Map d) = runExists (\(MapF f x) -> map (mapFT f) (toFormTree x)) d
toFormTree (Monadic x) = x >>= toFormTree >>= pure <<< Monadic <<< Identity
toFormTree (FormList d) =
  runExists (\(FormListF d is proof) ->
    (\d' fis -> FormList (mkExists (FormListF d' fis proof)))
    <$> (traverse toFormTree d)
    <*> (toFormTree is)) d
toFormTree (Metadata m x) = map (Metadata m) (toFormTree x)

-- | Returns the topmost untransformed single field, if one exists
toField :: forall v m a. FormTree Identity v m a -> Maybe (SomeField v)
toField (Ref _ x)      = toField x
toField (Pure x)       = Just (someField x)
toField (Ap _)         = Nothing
toField (Map d)        = runExists (\(MapF _ x) -> toField x) d
toField (Monadic x)    = toField $ unwrap x
toField (FormList d)   = Nothing
toField (Metadata _ x) = toField x

-- | Returns the topmost applicative or index trees if either exists
-- otherwise returns an empty list
children :: forall v m a. FormTree Identity v m a -> List (SomeForm v m)
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
popRef :: forall v m a. FormTree Identity v m a -> Tuple (Maybe String) (FormTree Identity v m a)
popRef form = case form of
  Ref r x ->
    Tuple (Just r) x
  Pure _ ->
    Tuple Nothing form
  Ap _ ->
    Tuple Nothing form
  Map d ->
    runExists (\(MapF g x) -> case popRef x of
      Tuple r form' ->
        Tuple r (mapFT g form')) d
  Monadic x ->
    popRef $ unwrap x
  FormList d ->
    Tuple Nothing form
  Metadata m x -> case popRef x of
    Tuple r form' ->
      Tuple r (Metadata m form')

-- | Return the first/topmost label of a form
getRef :: forall v m a. FormTree Identity v m a -> Maybe String
getRef = fst <<< popRef

getMetadata :: forall v m a. FormTree Identity v m a -> List FormMeta
getMetadata (Map d) = runExists (\(MapF _ x) -> getMetadata x) d
getMetadata (Monadic x) = getMetadata $ unwrap x
getMetadata (Metadata m x) = m <> getMetadata x
getMetadata _ = Nil

--------------------------------------------------------------------------------
-- | Retrieve the form(s) at the given path
lookupForm :: forall v m a. Path -> FormTree Identity v m a -> List (SomeForm v m)
lookupForm path = map fst <<< lookupFormMetadata path

--
lookupFormMetadata
  :: forall v m a.
     Path -> FormTree Identity v m a -> List (Tuple (SomeForm v m) (List FormMeta))
lookupFormMetadata path = go Nil path <<< someForm
  where
    go md path' (SomeForm form') =
      runExists (\(SomeFormF form) ->
        case path' of
          Nil ->
            (Tuple (someForm form) (getMetadata form <> md) : Nil)
          (r : rs) -> case popRef form of
              Tuple (Just r') stripped
                | r == r' && null rs -> (Tuple (someForm stripped) (getMetadata form <> md) : Nil)
                | r == r'            -> children form >>= go (getMetadata form <> md) rs
                | otherwise          -> Nil
              Tuple Nothing _        -> children form >>= go (getMetadata form <> md) (r : rs)) form'


formMapV :: forall v w m a. Monad m => (v -> w) -> FormTree Identity v m a -> FormTree Identity w m a
formMapV f (Ref r x) = Ref r $ formMapV f x
formMapV f (Pure x)  = Pure (fieldMapV f x)
formMapV f (Ap d)    = runDay (\i x y -> Ap $ day i (formMapV f x) (formMapV f y)) d
formMapV f (Map d)   = runExists (\(MapF g x) -> mapFT (g >=> pure <<< lmap f) (formMapV f x)) d
formMapV f (Monadic x) = formMapV f $ unwrap x
formMapV f (FormList x) =
  runExists (\(FormListF d is p) ->
    FormList (mkExists (FormListF (map (formMapV f) d) (formMapV f is) p))
  ) x
formMapV f (Metadata m x) = Metadata m $ formMapV f x

ann :: forall v a. Path -> V v a -> V (List (Tuple Path v)) a
ann path =
  let
    build :: v -> List (Tuple Path v)
    build x = Tuple path x : Nil
  in unV (invalid <<< build) pure

eval
  :: forall v m a. Monad m
  => Method -> Env m -> FormTree Identity v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval = eval' Nil

eval'
  :: forall v m a. Monad m
  => Path -> Method -> Env m -> FormTree Identity v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval' path method env form = case form of
  Ref r x -> eval' (path <> (r : Nil)) method env x

  Pure field -> do
    val <- env path
    let x = evalField method val field
    pure $ Tuple (pure x) $ do
      v <- val
      pure (Tuple path v)

  Ap d ->
    runDay (\i x y -> do
      Tuple x' inp1 <- eval' path method env x
      Tuple y' inp2 <- eval' path method env y
      pure (Tuple (i <$> x' <*> y') (inp1 <> inp2))) d

  Map d ->
    runExists (\(MapF f x) -> do
      Tuple x' inp <- eval' path method env x
      x_ <- bindV (pure x') (f >=> pure <<< ann path)
      pure (Tuple x_ inp)) d

  Monadic x ->
    eval' path method env $ unwrap x

  FormList d ->
    runExists (\(FormListF defs fis proof) -> do
      Tuple ris inp1 <- eval' path method env fis
      let ris' = unV Left Right ris
      case ris' of
        Left err -> pure (Tuple (invalid err) inp1)
        Right is -> do
          res <- traverse
            (\i -> eval' (path <> ((show i) : Nil))
                            method env $ defs `defaultListIndex` i) is
          case unzip res of
            Tuple results inps ->
              pure (Tuple (coerceSymm proof <$> sequence results) (inp1 <> concat inps))) d

  Metadata _ x ->
    eval' path method env x

forOptional :: forall v b a. Semigroup v => (a -> V v b) -> Maybe a -> V v (Maybe b)
forOptional f = maybe (pure Nothing) (unV invalid (pure <<< pure) <<< f)

type DefaultList a = NonEmpty List a

defaultListIndex :: forall a. DefaultList a -> Int -> a
defaultListIndex (x :| xs) i
  | i < 0     = x
  | otherwise = case drop i xs of
      Nil -> x
      (y : _) -> y
