module Shopie.Form.Halogen
  ( runFormH
  , runFormH'
  ) where

import Prelude

import Data.Map as M
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

import Halogen as H

import Shopie.Form.View (View, runFormPure)
import Shopie.Form.Internal.Form (Form)


type State s = { form :: M.Map String String | s}
type ErrorM s = { errors :: M.Map String String | s }

runFormH
  :: forall v s f g a. String -> Form v (H.ComponentDSL (State s) f g) a
  -> H.ComponentDSL (State s) f g (Tuple (View v) (Maybe a))
runFormH name form =
  H.gets (M.toUnfoldable <<< _.form) >>= runFormPure name form

runFormH'
  :: forall v s s' f f' g p a. String
  -> Form v (H.ParentDSL (State s) s' f f' g p) a
  -> H.ParentDSL (State s) s' f f' g p (Tuple (View v) (Maybe a))
runFormH' name form =
  H.gets (M.toUnfoldable <<< _.form) >>= runFormPure name form
