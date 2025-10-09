module Component.Logoff where

import Prelude

import AppTheme (themeColor, themeFont)
import CSS.Background (backgroundColor)
import CSS.Color (white)
import CSS.Common (center)
import CSS.Cursor (cursor, pointer)
import CSS.Display (display, flex)
import CSS.Flexbox (alignItems, column, flexDirection, justifyContent)
import CSS.Font (FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry (height, paddingBottom, paddingTop, width)
import CSS.Property (value)
import CSS.Size (rem, vw)
import Capability.Log (class Log, LogLevel(..), log, logEntry)
import Capability.Navigate (class Navigate, navigate)
import Component.Modal as Modal
import Component.Modal.Common as ModalCommon
import Component.Modal.Message as Message
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Api.Logoff (LogoffRequest(..), LogoffResponse(..), LogoffResults(..))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Env (Env)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Utils (apiCall)

type Input = Unit
type Output = Void

type State =
  { message :: String
  , errorMessage :: Maybe String
  }

data Action
  = Initialize
  | Click
  | ErrorModal (Modal.Output Message.Output)

type Query :: ∀ k. k -> Type
type Query = Const Void

type Slots = ( errorModal :: H.Slot (Modal.InnerQuery Message.Query) (Modal.Output Message.Output) Unit )
_errorModal = Proxy :: Proxy "errorModal"

component
  :: ∀ m
  . MonadAff m
  => MonadAsk Env m
  => Navigate m Route
  => Log m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> { message: "Logging off..."
                        , errorMessage: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize }
  }
  where
    handleAction
      :: Action
      -> H.HalogenM State Action
          Slots Output m Unit
    handleAction = case _ of
      Initialize -> do
        { userRef } <- ask
        loggedOnUser <- H.liftEffect $ Ref.read userRef
        loggedOnUser # maybe (pure unit) \ { authToken } -> do
          logoffResponse <- apiCall $ LogoffRequest { authToken }
          case logoffResponse of
            Left err -> H.modify_ _ { errorMessage = Just err }
            Right (LogoffResponse LogoffResultsFailure) ->
              H.modify_ _ { errorMessage = Just "Error logging off" }
            Right (LogoffResponse LogoffResultsSuccess) -> do
              log =<< logEntry Info "User logged off"
              { userRef } <- ask
              H.liftEffect $ Ref.write Nothing userRef
              H.modify_ _ { message = "Successfully Logged Off" }

      Click -> navigate $ Route.Logon

      ErrorModal output -> case output of
        Modal.Affirmative -> H.modify_ _ { errorMessage = Nothing }
        Modal.Negative -> H.modify_ _ { errorMessage = Nothing }
        Modal.InnerOutput _ -> pure unit


    render :: State -> H.ComponentHTML Action Slots m
    render { message, errorMessage } = HH.div [
      HC.style do
         display flex
         flexDirection column
         alignItems center
         justifyContent center
         paddingTop $ vw 0.65
      ]
      [
         HH.span
         [
           HC.style do
             color white
             paddingBottom (rem 1.0)
         ]
         [ HH.text message ]
      ,  HH.button
        [
          HC.style do
             backgroundColor themeColor
             themeFont
             fontWeight $ FontWeight $ value "500"
             fontSize $ vw 1.0
             width (rem 20.0)
             height $ vw 3.0
             color white
             cursor pointer
        , HE.onClick $ const Click
        ]
        [ HH.text "Logon" ]
      , (errorMessage # maybe (HH.text "") \msg ->
            HH.slot _errorModal unit
              (Modal.component ModalCommon.errorConfig Message.component) msg ErrorModal)
      ]
