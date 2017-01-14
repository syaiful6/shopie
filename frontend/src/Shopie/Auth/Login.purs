module Shopie.Auth.Login where

import Shopie.Prelude

import Control.Monad.Aff (later')
import Control.Monad.Aff.Bus as Bus

import Data.Map as M
import Data.String as S
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

import Text.Email.Validate as EV

import Shopie.Auth.Class (authenticate)
import Shopie.Auth.AuthF.Interpreter.Wiring (ForgotMessage(..))
import Shopie.Auth.Types (AuthResult(..), Email, Passwords, Creds, passwordCreds)
import Shopie.Button.Spinner (SpinnerS, SpinnerQuery(..), SpinnerSlot(..), spinner, mkSpinner)
import Shopie.Form ((.:), Form, text, check, viewStrError)
import Shopie.Form.Halogen as FH
import Shopie.ShopieM (class NotifyQ, Shopie, Wiring(..), notifyError, forgotten, notifyInfo)
import Shopie.Utils.Error (censorMF)


data LoginQuery a
  = UpdateEmail Email a
  | UpdatePasswords Passwords a

-- | Synonim for errors Map, the key part is the field name and value part is
-- | error message.
type FieldError = M.Map String String
type FormEnv = M.Map String String

type LoginState =
  { form :: FormEnv
  , errors :: FieldError
  }

-- | Initial state of `authLogin` component.
initialState :: LoginState
initialState =
  { form : M.empty
  , errors: M.empty
  }

-- | Synonim for ParentState used here
type LoginSP = H.ParentState LoginState SpinnerS LoginQuery SpinnerQuery Shopie SpinnerSlot

-- | Synonim for Component Query used here
type LoginQP = Coproduct LoginQuery (H.ChildF SpinnerSlot SpinnerQuery)

-- | Synonim for our Component DSL
type LoginDSL = H.ParentDSL LoginState SpinnerS LoginQuery SpinnerQuery Shopie SpinnerSlot

-- | Our HTML output
type LoginHTML = H.ParentHTML SpinnerS LoginQuery SpinnerQuery Shopie SpinnerSlot

-- | login Component
authLogin :: H.Component LoginSP LoginQP Shopie
authLogin = H.parentComponent { render, eval, peek: Just peek }

-- | The Component render function
render :: LoginState -> LoginHTML
render st = wrapper st.errors $ loginForm st

-- | The wrapper HTML, used so we don't nested the declaration of actual login
-- | form. This extra wrapper necessary just for styling purpose
wrapper :: forall p i. M.Map String String -> H.HTML p i -> H.HTML p i
wrapper m html =
  HH.div
    [ HP.class_ $ HH.className "sh-flow" ]
    [ HH.div
        [ HP.class_ $ HH.className "sh-flow-content-wrap" ]
        [ HH.div
            [ HP.class_ $ HH.className "sh-flow-content" ]
            [ html
            , HH.ul
                [ HP.class_ $ HH.className "main-error" ]
                (map renderError $ M.toUnfoldable m)
            ]
        ]
    ]
  where
    renderError :: Tuple String String -> H.HTML p i
    renderError (Tuple _ msg) = HH.li_ [ HH.text $ msg ]

-- | The actual HTML that produce our query algebra
loginForm :: LoginState -> LoginHTML
loginForm st@{ form } =
  HH.div
    [ HP.id_ "login"
    , HP.class_ $ HH.className "sh-signin"
    ]
    [ HH.div
        [ HP.class_ $ HH.className "form-group" ]
        [ HH.span
          [ HP.class_ $ HH.className "input-icon icon-mail" ]
          [ HH.input
              [ HP.class_ $ HH.className ("sh-input email" <> ("login.email" `existE` st.errors))
              , HP.inputType HP.InputEmail
              , HP.placeholder "Email Address"
              , HP.value $ fromMaybe "" $ M.lookup "login.email" form
              , HE.onValueChange $ HE.input UpdateEmail
              ]
          ]
        ]
    , HH.div
        [ HP.class_ $ HH.className "form-group" ]
        [ HH.span
          [ HP.class_ $ HH.className "input-icon icon-lock forgotten-wrap" ]
          [ HH.input
              [ HP.class_ $ HH.className ("sh-input password" <> ("login.passwords" `existE` st.errors))
              , HP.inputType HP.InputPassword
              , HP.placeholder "Passwords"
              , HP.value $ fromMaybe "" $ M.lookup "login.passwords" form
              , HE.onValueChange $ HE.input UpdatePasswords
              ]
          , renderSpinner "spinner-f" "forgotten-link btn btn-link" "Forgot?"
          ]
        ]
    , renderSpinner "spinner-l" "login btn btn-coffee btn-block" "Login in"
    ]

-- | Render spinner button component.
renderSpinner :: String -> String -> String -> LoginHTML
renderSpinner sl text c =
  HH.slot (SpinnerSlot sl) \_ ->
    { component: spinner, initialState: text `mkSpinner` c }

-- | Return " error" if the given key exists on a given map
existE :: forall k v. Ord k => k -> M.Map k v -> String
existE k m = maybe "" (const " error") $ M.lookup k m

eval :: LoginQuery ~> LoginDSL
eval (UpdateEmail em n) =
  FH.alter' (EV.isValid em) "login.email" "Invalid email"
  *> H.modify (\r -> r { form = M.insert "login.email" em r.form })
  $> n
eval (UpdatePasswords p n) =
  FH.alter' (not (S.null p)) "login.passwords" "Please enter your passwords"
  *> H.modify (\r -> r { form = M.insert "login.passwords" p r.form })
  $> n

peek :: forall a. H.ChildF SpinnerSlot SpinnerQuery a -> LoginDSL Unit
peek (H.ChildF p q) = case q, p of
  Submit _, SpinnerSlot "spinner-f" ->
    censorMF (runMaybeT handleForgotP) $ do
      errorN "(Error), Make sure enter valid email address." 10000.00
      H.query p (H.action (ToggleSpinner false)) $> unit
  Submit _, SpinnerSlot "spinner-l" -> do
    v <- FH.runFormH' "login" loginForm'
    case v of
      Tuple _ (Just d) ->
        runMaybeT (handleLogin d)
        *> H.query p (H.action (ToggleSpinner false)) $> unit
      Tuple view Nothing ->
        H.modify (_ { errors = M.fromFoldable (viewStrError view) })
        *> H.query p (H.action (ToggleSpinner false)) $> unit
  _, _ ->
    pure unit

-- Handle forgotten passwords button
handleForgotP :: MaybeT LoginDSL Unit
handleForgotP = do
  em' <- MaybeT $ H.gets (M.lookup "login.email" <<< _.form)
  let v = EV.isValid em'
  lift $ FH.alter' v "login.email" "Invalid email"
  -- make sure our validation success
  guard v
  lift $ H.liftH $ H.liftH $ forgotten em'
  Wiring { auth } <- lift $ H.liftH $ H.liftH ask
  -- race the result with default timeout, so we can recover the spinner
  res <- H.fromAff $ sequential $
    (parallel $ Bus.read auth.forgotBus) <|> (parallel $ later' 10000 (pure ForgotFailure))
  guard (res == ForgotSucces)
  lift $ H.set initialState
  infoN ("Email sent! Check your inbox in " <> em') 10000.00

handleLogin :: Creds -> MaybeT LoginDSL Unit
handleLogin cred = do
  res' <- authenticate cred
  case res' of
    Authenticated _ -> errorN "Login success!" 10000.00
    _ -> errorN "Login failed" 10000.00

loginForm' :: forall m. Monad m => Form String m Creds
loginForm' = passwordCreds
  <$> "email"     .: check "Invalid email" EV.isValid (text Nothing)
  <*> "passwords" .: check "Please enter your passwords" (not <<< S.null) (text Nothing)

infoN :: forall g. NotifyQ g => String -> Number -> g Unit
infoN msg n = notifyInfo msg $ Just (Milliseconds n)

errorN :: forall g. NotifyQ g => String -> Number -> g Unit
errorN msg n = notifyError msg $ Just (Milliseconds n)
