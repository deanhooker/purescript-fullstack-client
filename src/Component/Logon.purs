module Component.Logon where

import Prelude

import CSS (color, white)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC

type Input = Unit
type Output = Void
type State = {}
type Action = Void

type Query :: ∀ k. k -> Type
type Query = Const Void

type Slots :: ∀ k. Row k
type Slots = ()

component
  :: ∀ m
  . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> {}
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render {} = HH.span [ HC.style $ color white ] [ HH.text "Logon" ]
