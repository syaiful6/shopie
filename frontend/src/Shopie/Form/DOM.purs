module Shopie.Form.DOM
  ( runForm
  ) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.List (List(Nil), singleton)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (HTMLInputElement, htmlDocumentToParentNode, readHTMLInputElement)

import Shopie.Form.Formlet (Form)
import Shopie.Form.Types (Env, FormInput(TextInput))
import Shopie.Form.View (View, postForm, classify)

queryFieldForm
  :: forall m eff. MonadEff (dom :: DOM | eff) m
  => String -> m (Maybe HTMLInputElement)
queryFieldForm query = do
  mel <- liftEff $
    toMaybe <$>
      ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
  pure case mel of
    Nothing -> Nothing
    Just el -> either (const Nothing) Just $ runExcept $ readHTMLInputElement (toForeign el)

envDOM :: forall m eff. MonadEff (dom :: DOM | eff) m => Env m
envDOM path = do
  v <- queryFieldForm ("." <> classify path)
  case v of
    Nothing -> pure $ Nil
    Just x ->  liftEff (singleton <<< TextInput <$> value x)

runForm
  :: forall v m a eff. MonadEff (dom :: DOM | eff) m
  => String -> Form v m a -> m (Tuple (View v) (Maybe a))
runForm name form = postForm name form \_ -> pure envDOM
