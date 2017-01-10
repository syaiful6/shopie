module Test.Shopie.Form.Field
  ( bool
  , text
  , main
  ) where

import Prelude

import Data.List (List(Nil), (:))

import Test.QuickCheck (quickCheck, QC, (<?>))

import Shopie.Form.Types (Method(..), FormInput(..))
import Shopie.Form.Internal.Field as IF

text :: String -> IF.Field String String
text = IF.text

bool :: Boolean -> IF.Field String Boolean
bool = IF.bool

main :: forall eff. QC eff Unit
main = do
  quickCheck \b ->
    let
      result = IF.evalField Post Nil (bool b)
    in
      false == result <?> show b <> ": evalField bool post without input" <> show result

  quickCheck \b ->
    let
      result = IF.evalField Post (TextInput "any" : Nil) (bool b)
    in
      false == result <?> show b <> ": evalField bool post strange input" <> show result

  quickCheck \b ->
    let
      result = IF.evalField Post (TextInput "on" : Nil) (bool b)
    in
      true == result <?> show b <> ": evalField bool post correct input" <> show result

  quickCheck \b ->
    let
      result = IF.evalField Get Nil (bool b)
    in
      b == result <?> show b <> ": evalField bool get. Evaluated: " <> show result
