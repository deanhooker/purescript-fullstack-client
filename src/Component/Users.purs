module Component.Users where

import Prelude

import Capability.Log (class Log, LogLevel(..), log, logEntry)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Api.QueryUsers (QueryUsersFailureReason(..), QueryUsersRequest(..), QueryUsersResponse(..), QueryUsersResults(..))
import Data.Array (length)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Route (Route)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Entity.User (User)
import Env (Env)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Utils (apiCall)
import Web.HTML (window)
import Web.HTML.Window (alert)

type Input = Unit -- No input from parent
type Output = Void -- No output to parent

type State =
  { authorized :: Boolean
  , selectedUser :: Maybe User
  , users :: Array User
  }

data Action = Initialize

type Query :: ∀ k. k -> Type
type Query = Const Void

type Slots :: ∀ k. Row k
type Slots = ()

component
  :: forall m
  . MonadAff m
  => MonadAsk Env m
  => Navigate m Route
  => Log m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ ->
     { authorized: false
     , selectedUser: Nothing
     , users: []
     }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  } where
    handleAction
      :: Action
      -> H.HalogenM State Action
          Slots Output m Unit
    handleAction = case _ of
      Initialize -> do
        { userRef } <- ask
        loggedOnUser' <- liftEffect $ Ref.read userRef
        loggedOnUser' # maybe (pure unit) \ { authToken, admin } ->
          when admin do
            queryResponse <- apiCall $ QueryUsersRequest { authToken }
            case queryResponse of
              Left err -> alertError err
              Right (QueryUsersResponse
                     (QueryUsersResultsFailure { reason })) ->
                alertError $ "Query Users: " <> case reason of
                  NotAuthorized -> "Not Authorized"
                  NotAuthenticated -> "Not Authenticated"
              Right (QueryUsersResponse
                     (QueryUsersResultsSuccess
                      { users })) -> do
                H.modify_ _ { authorized = true, users = users }
        where
          alertError :: String -> H.HalogenM State Action Slots Output m Unit
          alertError msg = H.liftEffect $ window >>= alert msg

    render :: State -> H.ComponentHTML Action Slots m
    render { authorized, users, selectedUser } = HH.text $ "Found " <> show (length users) <> " users."
