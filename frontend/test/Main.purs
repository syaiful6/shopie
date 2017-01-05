module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Argonaut (encodeJson)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)

import Network.JsonApi.Resource (toResource)
import Network.JsonApi.Document (mkDocument)

import Shopie.User.Model (User, UserAttributes, user)
import Shopie.Validation.Validation (runValidation)

import Test.Validation (userV)

isaac :: User UserAttributes
isaac =
  user Nothing $
    { firstName: "Isaac"
    , lastName: "Newton"
    , username: "isaac"
    , email: "isaac@newton.com"
    }

neil :: User UserAttributes
neil =
  user (Just "22") $
    { firstName: "Neil"
    , lastName: ""
    , username: "nel22"
    , email: "neil@opas.com"
    }

anonym :: User UserAttributes
anonym =
  user Nothing $
    { firstName: ""
    , lastName: ""
    , username: ""
    , email: ""
    }

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "create resource"
  logShow $ encodeJson $ toResource isaac

  log "Create simple Document."
  logShow $ encodeJson $ mkDocument Nothing Nothing (L.singleton isaac)

  log "create collections"
  logShow $ encodeJson $ mkDocument Nothing Nothing [isaac, neil]

  log "run validation"
  v <- runValidation (userV "Hi" "there" "first" "valid@email.com") anonym
  logShow $ encodeJson $ toResource $ fst v
  logShow $ snd v

  log "run validation"
  v <- runValidation (userV "" "" "first" "invalidEmail") anonym
  logShow $ encodeJson $ toResource $ fst v
  logShow $ snd v
