module Shopie.Form.Internal.Form where

import Prelude

import Data.Identity (Identity(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Semigroup(V, unV, invalid)

import Unsafe.Coerce (unsafeCoerce)

import Shopie.Form.Internal.Field (Field(Singleton), evalField)
import Shopie.Form.Types (Env, FormInput(..), Method(..), Path, toPath)

data ApF t v m a b = ApF (FormTree t v m (b -> a)) (FormTree t v m b)

data MapF t v m a b = MapF (b -> m (V v a)) (FormTree t v m b)

type Form v m a = FormTree m v m a

data FormTree t v m a
  = Ref String (FormTree t v m a)
  | Pure (Field v a)
  | Ap (Exists (ApF t v m a))
  | Map (Exists (MapF t v m a))
  | Monadic (t (FormTree t v m a))

data SomeFormF v m a = SomeFormF (FormTree Identity v m a)

data SomeForm v m = SomeForm (Exists (SomeFormF v m))

someForm :: forall v m a. FormTree Identity v m a -> SomeForm v m
someForm ft = SomeForm (mkExists (SomeFormF ft))

instance functorFormTree :: (Monad m, Semigroup v) => Functor (FormTree t v m) where
  map f t = transform (\b -> pure (pure (f b))) t

instance appyFormTree :: (Monad m, Semigroup v) => Apply (FormTree t v m) where
  apply = apFT

instance applicativeFormTree :: (Monad m, Semigroup v) => Applicative (FormTree t v m) where
  pure = Pure <<< Singleton

mapFT :: forall t v m a b. (b -> m (V v a)) -> FormTree t v m b -> FormTree t v m a
mapFT f x = Map (mkExists (MapF f x))

apFT :: forall t v m a b. FormTree t v m (b -> a) -> FormTree t v m b -> FormTree t v m a
apFT f x = Ap (mkExists (ApF f x))

transform
  :: forall t v m a b
   . Monad m
  => (b -> m (V v a))
  -> FormTree t v m b
  -> FormTree t v m a
transform f (Map mf) = runExists (\(MapF g x) -> mapFT (\y -> g y `bindV` f) x) mf
transform f x = mapFT f x

monadic :: forall v m a. m (Form v m a) -> Form v m a
monadic = Monadic

pushRef :: forall t v m a. String -> FormTree t v m a -> FormTree t v m a
pushRef = Ref

infixr 5 pushRef as .:

bindV :: forall v m a b. Monad m => m (V v a) -> (a -> m (V v b)) -> m (V v b)
bindV m f = m >>= unV (pure <<< invalid) f

toFormTree :: forall v m a. Monad m => Form v m a -> m (FormTree Identity v m a)
toFormTree (Ref r x) = map (Ref r) (toFormTree x)
toFormTree (Pure x) = pure (Pure x)
toFormTree (Ap d) = runExists (\(ApF x y) -> apFT <$> (toFormTree x) <*> (toFormTree y)) d
toFormTree (Map d) = runExists (\(MapF f x) -> map (mapFT f) (toFormTree x)) d
toFormTree (Monadic x) = x >>= toFormTree >>= pure <<< Monadic <<< Identity

children :: forall v m a. FormTree Identity v m a -> List (SomeForm v m)
children (Ref _ x) = children x
children (Pure x) = Nil
children (Ap d) = runExists (\(ApF x y) -> (someForm x : someForm y : Nil)) d
children (Map d) = runExists (\(MapF _ x) -> children x) d
children (Monadic x) = children $ unwrap x

ann :: forall v a. Path -> V v a -> V (List (Tuple Path v)) a
ann path =
  let
    build :: v -> List (Tuple Path v)
    build x = Tuple path x : Nil
  in unV (invalid <<< build) pure

eval
  :: forall v m a
   . Monad m
  => Method
  -> Env m
  -> FormTree Identity v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval = eval' Nil

eval'
  :: forall v m a
   . Monad m
  => Path
  -> Method
  -> Env m
  -> FormTree Identity v m a
  -> m (Tuple (V (List (Tuple Path v)) a) (List (Tuple Path FormInput)))
eval' path method env form = case form of
  Ref r x -> eval' (path <> (r : Nil)) method env x

  Pure field -> do
    val <- env path
    let x = evalField method val field
    pure $ Tuple (pure x) $ do
      v <- val
      pure (Tuple path v)

  Ap d -> runExists (\(ApF x y) -> do
    Tuple x' inp1 <- eval' path method env x
    Tuple y' inp2 <- eval' path method env y
    pure (Tuple (x' <*> y') (inp1 <> inp2))
  ) d

  Map d -> runExists (\(MapF f x) -> do
    Tuple x' inp <- eval' path method env x
    x_ <- bindV (pure x') (f >=> pure <<< ann path)
    pure (Tuple x_ inp)
  ) d

  Monadic x -> eval' path method env $ unwrap x

forOptional :: forall v b a. Semigroup v => (a -> V v b) -> Maybe a -> V v (Maybe b)
forOptional f = maybe (pure Nothing) (unV invalid (pure <<< pure) <<< f)
