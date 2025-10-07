module Component.Message where

import Prelude

import AppTheme (paperColor, themeFont)
import CSS.Background (backgroundColor)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC

type Input = String
type Output = Void
type Action = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slots :: forall k. Row k
type Slots = ()

type State = { message :: String }

component
  :: forall m
  . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \message -> { message }
  , render
  , eval: H.mkEval H.defaultEval
  } where
    render :: State -> H.ComponentHTML Action Slots m
    render { message } =
      HH.div [
        HC.style do
           themeFont
           backgroundColor paperColor
      ]
      [ HH.text message ]
