module Shopie.Application where

import Shopie.Prelude

import Control.Monad.Aff.Bus as Bus
import Control.Error.Util (hush)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.List as L
import Data.Path.Pathy ((</>), file, dir, rootDir)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Network.JsonApi (unDocument, fromResource)

import Shopie.Auth (SignInMessage(..), maybeAuthId)
import Shopie.Auth.Login as AL
import Shopie.Halogen.EventSource (raise')
import Shopie.Notification.List as NL
import Shopie.Route.Types as RT
import Shopie.Render.StateMode as ST
import Shopie.ShopieM (Shopie, Wiring(..), liftQyson)
import Shopie.User.Model as UM
import Shopie.User.Profile as UP

import Qyson.QysonF (readFile, jsonModeApi)


data AppQ a
  = GetRoute (RT.Locations -> a)
  | GetUser (Maybe (UM.User UM.UserAttributes) -> a)
  | ObserveAuth SignInMessage a
  | Init a
  | Move RT.Locations a
  | UpdateUser (Maybe (UM.User UM.UserAttributes)) a

type AppS =
  { route :: RT.Locations
  , user :: Maybe (UM.User UM.UserAttributes)
  , stateMode :: ST.StateMode
  }

anonym :: UM.User UM.UserAttributes
anonym = UM.user Nothing $
           { firstName: "anonym"
           , lastName: ""
           , username: ""
           , email: "anonym@anonym.com"
           }

initialState :: AppS
initialState =
  { route: RT.Login
  , user: Nothing
  , stateMode: ST.Loading
  }

data NotifListSlot = NotifListSlot
derive instance eqNotifListSlot :: Eq NotifListSlot
derive instance ordNotifListSlot :: Ord NotifListSlot

newtype ProfileSlot = ProfileSlot String
derive instance eqProfileSlot :: Eq ProfileSlot
derive instance ordProfileSlot :: Ord ProfileSlot

data LoginSlot = LoginSlot
derive instance eqLoginSlot :: Eq LoginSlot
derive instance ordLoginSlot :: Ord LoginSlot

type ChildS    = Either NL.StateP (Either UP.Profile AL.LoginSP)
type ChildQ    = Coproduct NL.QueryP (Coproduct UP.ProfileQ AL.LoginQP)
type ChildSlot = Either NotifListSlot (Either ProfileSlot LoginSlot)

-- | path to notification
cpN :: ChildPath NL.StateP ChildS NL.QueryP ChildQ NotifListSlot ChildSlot
cpN = cpL

-- | path to profile
cpP :: ChildPath UP.Profile ChildS UP.ProfileQ ChildQ ProfileSlot ChildSlot
cpP = cpR :> cpL

-- | path to login
cpLo :: ChildPath AL.LoginSP ChildS AL.LoginQP ChildQ LoginSlot ChildSlot
cpLo = cpR :> cpR

-- | Parent state
type AppSP  = H.ParentState AppS ChildS AppQ ChildQ Shopie ChildSlot

-- | Parent Query synonim
type AppQP   = Coproduct AppQ (H.ChildF ChildSlot ChildQ)

-- | App HTML output
type AppHTML = H.ParentHTML ChildS AppQ ChildQ Shopie ChildSlot

-- | Component DSL
type AppDSL = H.ParentDSL AppS ChildS AppQ ChildQ Shopie ChildSlot

-- | Component app
app :: H.Component AppSP AppQP Shopie
app =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render :: AppS -> AppHTML
render s =
  HH.div
    [ HP.class_ $ HH.className "sh-app" ]
    [ renderNotification
    , HH.div
        [ HP.class_ $ HH.className "sh-viewport" ]
        [ showLoadOrView s ]
    ]

renderNotification :: AppHTML
renderNotification = HH.slot' cpN NotifListSlot $
  \_-> { component: NL.list, initialState: H.parentState NL.initialState }

showLoadOrView :: AppS -> AppHTML
showLoadOrView { stateMode, route, user } = case stateMode of
  ST.Loading ->
    ST.loadingRender
  ST.Ready ->
    renderView route user
  ST.Error error ->
    HH.div_ [ HH.text "error" ]

renderView :: RT.Locations -> Maybe (UM.User UM.UserAttributes) -> AppHTML
renderView RT.Home _ =
  HH.div_ [ HH.text "Home" ]
renderView RT.Login _ =
  HH.slot' cpLo LoginSlot $
    \_ -> { component: AL.authLogin, initialState: H.parentState AL.initialState }
renderView RT.Logout _ =
  HH.div_ [ HH.text "See ya!" ]
renderView RT.Profile u =
  case u of
    Just u' ->
      let p = UP.fromUser u'
      in HH.slot' cpP (ProfileSlot p.userId) $
           \_ -> { component: UP.profile, initialState: p }
    Nothing ->
      HH.div_ [ HH.text "Login to edit your profile" ]
renderView (RT.Order c) _ =
  HH.div_ [ HH.text ("Order" <> show c) ]
renderView RT.NotFound _ =
  HH.div_ [ HH.text "NotFound" ]

eval :: AppQ ~> AppDSL
eval (Init next) = do
  wrapAction $ raiseUserUpdate =<< runMaybeT maybeAuthenticate
  Wiring { auth } <- H.liftH $ H.liftH ask
  forever (raise' <<< H.action <<< ObserveAuth =<< H.fromAff (Bus.read auth.signinBus))
eval (ObserveAuth msg next) = do
  case msg of
    SignInSuccess ->
      wrapAction (runMaybeT maybeAuthenticate >>= raiseUserUpdate) $> next
    _ ->
      pure next
eval (Move RT.Login next) = do
  u <- H.gets (_.user)
  unless (isJust u) $ H.modify (_ { route = RT.Login, stateMode = ST.Ready })
  pure next
eval (Move loc next) = do
  u <- H.gets (_.user)
  when (isJust u) $ H.modify (_ { route = loc, stateMode = ST.Ready })
  pure next
eval (UpdateUser u next) =
  H.modify (_ { user = u }) $> next
eval (GetUser reply) = reply <$> H.gets (_.user)
eval (GetRoute reply) = reply <$> H.gets (_.route)

raiseUserUpdate :: Maybe Json -> AppDSL Unit
raiseUserUpdate = case _ of
  Nothing ->
    pure unit
  Just x -> do
    raise' $ H.action $ UpdateUser (decodeUser x)
    raise' $ H.action $ Move RT.Profile

maybeAuthenticate :: MaybeT AppDSL Json
maybeAuthenticate = do
  i <- MaybeT $ maybeAuthId
  MaybeT $ hush <$> (H.liftH $ H.liftH $ liftQyson $
    readFile jsonModeApi loc Nothing)
  where
    loc = rootDir </> dir "users" </> dir "me" </> file ""

decodeUser :: Json -> Maybe (UM.User UM.UserAttributes)
decodeUser =
  hush <<< decodeJson >=> L.head <<< _.resources <<< unDocument >=> pure <<< fromResource

wrapAction :: forall a. AppDSL a -> AppDSL a
wrapAction fa =
  H.modify (_ { stateMode = ST.Loading })
  *> fa
  <* H.modify (_ { stateMode = ST.Ready })
