module Component.Modal.Message where

import Prelude

import AppTheme (paperColor, themeFont)
import CSS.Background (backgroundColor)
import Component.Modal (InnerOutput(..), InnerQuery(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC

type Input = String
type Output = Void
type Action = Void

type Query = Void

type Slots :: forall k. Row k
type Slots = ()

type State = { message :: String }

component
  :: forall m
  . MonadAff m
  => H.Component (InnerQuery Query) Input (InnerOutput Output) m
component = H.mkComponent
  { initialState: \message -> { message }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleQuery = handleQuery }
  } where
    handleQuery :: forall a
                . InnerQuery Query a
                -> H.HalogenM State Action Slots (InnerOutput Output) m (Maybe a)
    handleQuery = case _ of
      AffirmativeClicked a -> do
        H.raise ParentAffirmative
        pure $ Just a
      NegativeClicked a -> do
        H.raise ParentNegative
        pure $ Just a
      _ -> pure Nothing

    render :: State -> H.ComponentHTML Action Slots m
    render { message } =
      HH.div [
        HC.style do
           themeFont
           backgroundColor paperColor
      ]
      [ HH.text message ]
