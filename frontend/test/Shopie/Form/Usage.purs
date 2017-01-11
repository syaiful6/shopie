module Test.Shopie.Form.Usage where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.List (List(Nil), (:), filter)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd, fst)

import Control.Monad.Reader (Reader, runReader, ask)

import Shopie.Form.Formlet ((.:), FormEncType, Form, Formlet, monadic, choice, choiceWith, text)
import Shopie.Form.Types (Env, FormInput(TextInput), fromPath)
import Shopie.Form.View (postForm)

data TransMode
  = Air
  | AnimalPowered
  | Rail
  | Road
  | Water

derive instance eqTransMode :: Eq TransMode
derive instance ordTransMode :: Ord TransMode

instance showTransMode :: Show TransMode where
  show Air = "Air"
  show AnimalPowered = "Animal-Powered"
  show Rail = "Rail"
  show Road = "Road"
  show Water = "Water"

newtype Transportation = Transportation
  { name :: String
  , transid :: String
  }

derive instance eqTransportation :: Eq Transportation
derive instance ordTransportation :: Ord Transportation

instance showTransportation :: Show Transportation where
  show (Transportation { name, transid }) = "Transportation " <> name <> " " <> transid

transModeChoices :: NonEmpty List (Tuple TransMode String)
transModeChoices = mode <#> \m -> Tuple m (show m)
  where
    mode =
      ( Air
      :|
      ( AnimalPowered
      : Rail
      : Road
      : Water
      : Nil
      ))

transport :: String -> String -> Transportation
transport name transid = Transportation { name: name, transid: transid }

type TravelM = Reader (NonEmpty List Transportation)

garuda :: Transportation
garuda = transport "Garuda" "garuda-airlines"

merpati :: Transportation
merpati = transport "Merpati" "merpati-airlines"

argoLawu :: Transportation
argoLawu = transport "Argo Lawu" "train-al"

runTravelM :: forall a. TravelM a -> a
runTravelM = flip runReader (garuda :| (merpati : argoLawu : Nil))

buildTransChoice :: Transportation -> Tuple String (Tuple Transportation String)
buildTransChoice t@(Transportation { transid, name }) =
  Tuple transid (Tuple t name)

transModeForm :: forall m. Monad m => Form String m TransMode
transModeForm = choice transModeChoices Nothing

transportationForm :: Formlet String TravelM Transportation
transportationForm def = monadic do
  transports <- ask
  pure $ choiceWith (buildTransChoice <$> transports) def

newtype Traveller = Traveller
  { name :: String
  , destination :: String
  , trans :: Transportation
  , transMode :: TransMode
  }

derive instance eqTraveller :: Eq Traveller
derive instance ordTraveller :: Ord Traveller

instance showTraveller :: Show Traveller where
  show (Traveller { name, destination, trans, transMode }) =
    "Traveller\n" <>
      ( "{ name: " <> name <> "\n"
      <> ", destination: " <> destination <> "\n"
      <> ", trans: " <> show trans <> "\n"
      <> ", transMode: " <> show transMode <> "\n")

travelerForm :: Formlet String TravelM Traveller
travelerForm def = { name: _, destination: _, trans: _, transMode: _}
  <$> "name"          .: text Nothing
  <*> "destination"   .: text Nothing
  <*> "trans"         .: transportationForm Nothing
  <*> "transMode"     .: transModeForm
  <#> Traveller

testEnv :: forall m. Monad m => List (Tuple String String) -> FormEncType -> m (Env m)
testEnv input ec = pure $ \key -> pure $ map (TextInput <<< snd) $
  filter (eq (fromPath key) <<< fst) input

testCase :: forall a b eff. (Show a, Eq b, Show b) => a -> b -> b -> Eff (console :: CONSOLE | eff) Unit
testCase name expect actual =
  if expect == actual
    then pure unit
    else log ("Fail: " <> show name <> "\nexpect: " <> show expect <> "\nbut actual is " <> show actual)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let
    aryaW = Traveller
      ({ name: "Arya Wiraraja"
       , destination: "Nangka"
       , trans: garuda
       , transMode: Air
       })

  testCase "Simple postForm" (Just aryaW) $
    snd $ runTravelM $ postForm "f" (travelerForm Nothing) $ testEnv
      ( Tuple "f.name" "Arya Wiraraja"
      : Tuple "f.destination" "Nangka"
      : Tuple "f.trans" "garuda-airlines"
      : Tuple "f.transMode" "Air"
      : Nil
      )
