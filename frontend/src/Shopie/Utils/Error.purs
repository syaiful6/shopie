module Shopie.Utils.Error where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Except.Trans (ExceptT(..))

import Halogen as H

import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Map as M
import Data.Maybe (Maybe, maybe)


foldExceptT :: forall m a b c. Monad m => (a -> m c) -> (b -> m c) -> ExceptT a m b -> m c
foldExceptT f g (ExceptT m) =  m >>= either f g

bimapExceptT
  :: forall m a b f e
   . Functor m
  => (e -> f)
  -> (a -> b)
  -> ExceptT e m a
  -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (map (bimap f g) m)

-- |
foldMaybeT :: forall m a b. Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
foldMaybeT mb hb (MaybeT m) = m >>= maybe mb hb

-- | Lift maybe value to an Applicative. If the given maybe Nothing then use
-- | the first argument.
censor :: forall m a. Applicative m => m a -> Maybe a -> m a
censor h = maybe h pure

-- | flipped version of censor
censorF :: forall m a. Applicative m => Maybe a -> m a -> m a
censorF = flip censor

censorM :: forall m a. Monad m => m a -> m (Maybe a) -> m a
censorM h mm = censor h =<< mm

censorMF :: forall m a. Monad m => m (Maybe a) -> m a -> m a
censorMF = flip censorM

recover :: forall m a b. Applicative m => (b -> m a) -> Either b a -> m a
recover h = either h pure

recoverF :: forall m a b. Applicative m => Either b a -> (b -> m a) -> m a
recoverF = flip recover

recoverM :: forall m a b. Monad m => (b -> m a) -> m (Either b a) -> m a
recoverM h me = recover h =<< me

recoverMF :: forall m a b. Monad m => m (Either b a) -> (b -> m a) -> m a
recoverMF = flip recoverM

printError :: forall k v. Ord k => k -> M.Map k v -> String
printError k = maybe "" (const " error") <<< M.lookup k

type ErrorM s = { errors :: M.Map String String | s }

alter :: forall s f g. Boolean -> String -> String -> H.ComponentDSL (ErrorM s) f g Unit
alter pred path message =
  let upd = if pred then M.delete path else M.insert path message
  in
    H.modify (\r -> r { errors = upd r.errors })

alter'
  :: forall s s' f f' g p. Boolean -> String -> String
  -> H.ParentDSL (ErrorM s) s' f f' g p Unit
alter' pred path message =
  let upd = if pred then M.delete path else M.insert path message
  in
    H.modify (\r -> r { errors = upd r.errors })
