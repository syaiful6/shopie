module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Argonaut (encodeJson)
import Data.List as L
import Data.Maybe (Maybe(..))

import Network.JsonApi.Resource (toResource)
import Network.JsonApi.Document (mkDocument)

import Shopie.User.Model (User, UserR, UserAttributes, user)
import Shopie.Validation.Validation (censorV)

import Test.Shopie.Validation (userV, userV')

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

invalidUserR :: UserR
invalidUserR =
  { firstName: ""
  , lastName: "foo"
  , username: "bar"
  , email: "invalid"
  }

validUserR :: UserR
validUserR =
  { firstName: "first"
  , lastName: "foo"
  , username: "bar"
  , email: "valid@domain.com"
  }

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "create resource"
  logShow $ encodeJson $ toResource isaac

  log "Create simple Document."
  logShow $ encodeJson $ mkDocument Nothing Nothing (L.singleton isaac)

  log "create collections"
  logShow $ encodeJson $ mkDocument Nothing Nothing [isaac, neil]

  log "run validation: the error result should collect all error"
  iv <- censorV (userV invalidUserR) anonym
  logShow $ encodeJson <<< toResource <$> iv

  log "run validation: the error result should empty"
  v <- censorV (userV validUserR) anonym
  logShow $ encodeJson <<< toResource <$> v

  log "run validation directly on the data structure (invalid)"
  ix <- censorV userV' (user Nothing invalidUserR)
  logShow $ encodeJson <<< toResource <$> ix

  log "run validation directly on the data structure (valid)"
  r <- censorV userV' (user Nothing validUserR)
  logShow $ encodeJson <<< toResource <$> r
