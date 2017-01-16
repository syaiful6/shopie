module Shopie.User.Profile where

import Shopie.Prelude

import Data.Map as M
import Data.String as S

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH

import Text.Email.Validate as EV

import Shopie.Form ((.:), Form, text, check, viewStrError)
import Shopie.Form.View (classify')
import Shopie.Form.DOM (runForm)
import Shopie.ShopieM (Shopie)
import Shopie.User.Model (User, UserAttributes, user, unAttr)
import Shopie.Utils.Error (alter, printError)


data ProfileQ a
  = Save a
  | UpdateFirstName String a
  | UpdateLastName String a
  | UpdateEmail String a
  | UpdateUsername String a

type UserId = String

type Profile =
  { userId :: UserId
  , firstName :: String
  , lastName :: String
  , username :: String
  , email :: String
  , errors :: M.Map String String
  }

-- | Profile component
profile :: H.Component Profile ProfileQ Shopie
profile = H.component { render, eval }

render :: Profile -> H.ComponentHTML ProfileQ
render u =
  HH.section
    [ HP.class_ $ HH.className "sh-view" ]
    [ renderHeader u
    , renderForm u
    ]

eval :: ProfileQ ~> H.ComponentDSL Profile ProfileQ Shopie
eval (UpdateFirstName fn next) =
  alter (not (S.null fn)) "profile.firstName" "firstName field cant be empty"
  *> H.modify (_ { firstName = fn }) $> next
eval (UpdateLastName ln next) = H.modify (_ { lastName = ln }) $> next
eval (UpdateEmail email next) =
  alter (EV.isValid email) "profile.email" "Invalid email"
  *> H.modify (_ { email = email }) $> next
eval (UpdateUsername um next) = H.modify (_ { username = um }) $> next
eval (Save next) = do
  v <- H.fromEff <<< runForm "profile" =<< profileForm <$> H.gets (_.userId)
  case v of
    Tuple _ (Just p) ->
      pure next
    Tuple vi Nothing ->
      H.modify (_ { errors = M.fromFoldable (viewStrError vi) }) $> next

renderHeader :: Profile -> H.ComponentHTML ProfileQ
renderHeader u =
  HH.header
    [ HP.class_ $ HH.className "view-header" ]
    [ HH.h2
        [ HP.class_ $ HH.className "view-title" ]
        [ HH.span_ $ [ HH.text (u.firstName <> " " <> u.lastName) ]]
    , HH.section
        [ HP.class_ $ HH.className "view-actions" ]
        [ HH.button
            [ HP.class_ $ HH.className "btn btn-blue"
            , HE.onClick (\_ -> HEH.preventDefault *> HEH.stopPropagation $> Just (H.action Save))
            ]
            [ HH.text "Save" ]
        ]
    ]

inputCls :: String -> M.Map String String -> String
inputCls p e =
  classify' p <> " sh-input" <> (p `printError` e)

renderForm :: Profile -> H.ComponentHTML ProfileQ
renderForm u@{ errors } =
  HH.section
    [ HP.class_ $ HH.className "view-container settings-user" ]
    [ HH.figure
        [ HP.class_ $ HH.className "user-cover" ]
        [ HH.button
            [ HP.class_ $ HH.className "btn btn-default user-cover-default" ]
            [ HH.text "Change cover" ]
        ]
    , HH.div
        [ HP.class_ $ HH.className "user-profile" ]
        [ HH.fieldset
            [ HP.class_ $ HH.className "user-details-top" ]
            [ HH.div
                [ HP.id_ "image"
                , HP.class_ $ HH.className "img"
                ]
                [ HH.text "Edit profile" ]
            ]
        , HH.div
            [ HP.class_ $ HH.className "first-form-group form-group" ]
            [ HH.label
                [ HP.for "profile-username" ]
                [ HH.text "User Name" ]
            , HH.input
                [ HP.id_ "profile-username"
                , HP.class_ $ HH.className (inputCls "profile.username" errors)
                , HP.placeholder "User name"
                , HP.value u.username
                , HE.onValueChange $ HE.input UpdateUsername
                ]
            ]
        , HH.fieldset
            [ HP.class_ $ HH.className "user-details-bottom" ]
            [ HH.div
                [ HP.class_ $ HH.className "form-group" ]
                [ HH.label
                    [ HP.for "profile-firstName" ]
                    [ HH.text "First Name" ]
                , HH.input
                    [ HP.id_ "profile-firstName"
                    , HP.class_ $ HH.className (inputCls "profile.firstName" errors)
                    , HP.placeholder "First name"
                    , HP.value u.firstName
                    , HE.onValueChange $ HE.input UpdateFirstName
                    ]
                ]
            , HH.div
                [ HP.class_ $ HH.className "form-group" ]
                [ HH.label
                    [ HP.for "profile-lastName" ]
                    [ HH.text "Last name" ]
                , HH.input
                    [ HP.id_ "profile-lastName"
                    , HP.class_ $ HH.className (inputCls "profile.lastName" errors)
                    , HP.placeholder "Last name"
                    , HP.value u.lastName
                    , HE.onValueChange $ HE.input UpdateLastName
                    ]
                ]
            , HH.div
                [ HP.class_ $ HH.className "form-group" ]
                [ HH.label
                    [ HP.for "profile-email" ]
                    [ HH.text "Email" ]
                , HH.input
                    [ HP.id_ "profile-email"
                    , HP.class_ $ HH.className (inputCls "profile.email" errors)
                    , HP.placeholder "User email"
                    , HP.value u.email
                    , HE.onValueChange $ HE.input UpdateEmail
                    ]
                ]
            ]
        ]
    ]

profileForm :: forall m. Monad m => UserId -> Form String m (User UserAttributes)
profileForm id = { firstName: _, lastName: _, email: _, username: _ }
  <$> "firstName"   .: check "First name field cant be empty" (not <<< S.null) (text Nothing)
  <*> "lastName"    .: text Nothing
  <*> "email"       .: check "Invalid email" EV.isValid (text Nothing)
  <*> "username"    .: text Nothing
  <#> user (Just id)

fromUser :: User UserAttributes -> Profile
fromUser u =
  let
    a = unAttr $ snd $ unwrap u
    ident = fromMaybe "" <<< fst <<< unwrap
  in
    { userId: ident u
    , firstName: a.firstName
    , lastName: a.lastName
    , email: a.email
    , username: a.username
    , errors: M.empty
    }

toUser :: Profile -> User UserAttributes
toUser p =
  let
    attr =
      { firstName: p.firstName
      , lastName: p.lastName
      , email: p.email
      , username: p.username
      }
  in
    user (Just p.userId) attr
