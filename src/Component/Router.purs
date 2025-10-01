module Component.Router where

import Prelude

import CSS (color, white)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Type.Proxy (Proxy(..))

type Input = Unit
type Output = Void

data Query a = Navigate Route a

type State = { route :: Route }

data Action
  = Void

type PageSlot = H.Slot (Const Void) Void Unit

type Slots =
  ( logon          :: PageSlot
  , logoff         :: PageSlot
  , users          :: PageSlot
  , changePassword :: PageSlot
  )

_logon          = Proxy :: Proxy "logon"
_logoff         = Proxy :: Proxy "logoff"
_users          = Proxy :: Proxy "users"
_changePassword = Proxy :: Proxy "changePassword"

component :: forall m. H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> { route: Logon}
  , render
  , eval: H.mkEval H.defaultEval {
        handleQuery = handleQuery
      }
  } where
    render :: State -> H.ComponentHTML Action Slots m
    render { route } = case route of
      Logon -> HH.span [ HC.style $ color white ] [ HH.text "Logon" ]
      Logoff -> HH.span [ HC.style $ color white ] [ HH.text "Logoff" ]
      Users _ -> HH.span [ HC.style $ color white ] [ HH.text "Users" ]
      ChangePassword ->
        HH.span [ HC.style $ color white ] [ HH.text "ChangePassword" ]

    handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
    handleQuery = case _ of
      Navigate route _ -> H.modify_ _ { route = route } *> pure Nothing
