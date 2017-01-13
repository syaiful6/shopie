module Shopie.Form.DOM where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (HTMLInputElement, htmlDocumentToParentNode, readHTMLInputElement)

import Shopie.Form.Formlet (FormEncType)
import Shopie.Form.Types (Env, FormInput(TextInput), fromPath)


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

envDOM
  :: forall m eff. MonadEff (dom :: DOM | eff) m
  => FormEncType -> m (Env m)
envDOM enc = pure $ \key -> do
  v <- queryFieldForm $ fromPath key
  case v of
    Nothing -> pure Nil
    Just x -> (pure <<< TextInput) <$> value x
