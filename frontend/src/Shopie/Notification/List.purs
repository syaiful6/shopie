module Shopie.Notification.List where

import Shopie.Prelude

import Control.Monad.Aff (later')
import Control.Monad.Aff.Bus as Bus

import Data.Int as Int
import Data.Map as M
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (Tuple3, tuple3, get1, get2, get3)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Math as Math

import Shopie.Halogen.EventSource (forkQuery', raise')
import Shopie.Notification.Item (notification, NotifQuery(..), NotificationItem)
import Shopie.ShopieM (Shopie, Wiring(..))
import Shopie.ShopieM.Notification as QN


data ListQuery a
  = Init a
  | Push QN.Notification a
  | RemoveItem NotifId a
  | RemoveAll a

type NotifId = Int
type Message = String

-- | Tuple
type Notify = Tuple3 NotifId QN.Level Message

type NotifList =
  { notifications :: M.Map NotifId Notify
  , nextId :: NotifId
  }

initialState :: NotifList
initialState =
  { notifications: M.empty
  , nextId: 0
  }

newtype NotifSlot = NotifSlot NotifId
derive instance eqNotifSlot :: Eq NotifSlot
derive instance ordNotifSlot :: Ord NotifSlot

-- | Synonim to make typing more readable
type StateP = H.ParentState NotifList NotificationItem ListQuery NotifQuery Shopie NotifSlot
type QueryP = Coproduct ListQuery (H.ChildF NotifSlot NotifQuery)
type NotifListDSL = H.ParentDSL NotifList NotificationItem ListQuery NotifQuery Shopie NotifSlot
type NotifListHTML = H.ParentHTML NotificationItem ListQuery NotifQuery Shopie NotifSlot

mkNotif :: QN.Level -> Message -> NotificationItem
mkNotif lvl msg =
  { message: msg
  , level: lvl
  , removed: false
  }

list :: H.Component StateP QueryP Shopie
list =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just peek
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render :: NotifList -> NotifListHTML
render st =
  HH.div
    [ HP.class_ $ HH.className "sh-notifications" ]
    (foldMap renderMessage st.notifications)

renderMessage :: Notify -> Array NotifListHTML
renderMessage t =
  [ HH.slot (NotifSlot $ get1 t) \_ ->
    { component: notification, initialState: get2 t `mkNotif` get3 t }
  ]

eval :: ListQuery ~> NotifListDSL
eval (Init next) = do
  Wiring { notify } <- H.liftH $ H.liftH ask
  forever (raise' <<< H.action <<< Push =<< H.fromAff (Bus.read notify))
eval (Push (QN.Notification { level, message, timeout}) next) = do
  i <- H.gets (_.nextId) <* H.modify (addNotification level message)
  case timeout of
    Nothing -> pure next
    Just (Milliseconds ms) ->
      let d = Int.floor $ Math.max ms zero in
      forkQuery' (later' d (pure i)) RemoveItem $> next
eval (RemoveItem i next) = H.modify (removeMessage (NotifSlot i)) $> next
eval (RemoveAll next) = H.queryAll (H.action (ToggleRemoved true)) $> next

peek:: forall a. H.ChildF NotifSlot NotifQuery a -> NotifListDSL Unit
peek (H.ChildF p q) = case q of
  Remove _ ->
    H.modify (removeMessage p)
  _ -> pure unit

addNotification :: QN.Level -> Message -> NotifList -> NotifList
addNotification lvl msg st =
  st { nextId = st.nextId + 1
     , notifications = M.insert st.nextId (tuple3 st.nextId lvl msg) st.notifications
     }

removeMessage :: NotifSlot -> NotifList -> NotifList
removeMessage (NotifSlot t) st =
  st { notifications = M.delete t st.notifications }
