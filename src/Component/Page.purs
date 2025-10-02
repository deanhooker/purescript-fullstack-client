module Component.Page where

import Prelude

import AppTheme (themeColor, paperColor, themeFont)
import CSS as CSS
import CSS.Background (backgroundColor)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (center)
import CSS.Cursor (cursor, pointer)
import CSS.Display (display, zIndex, position, fixed, flex)
import CSS.Flexbox (flexDirection, row, flexStart, flexEnd, flexBasis, flexShrink, flexGrow, alignItems, justifyContent)
import CSS.Font ( FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry ( padding, paddingTop, paddingLeft, paddingRight, width, height, minHeight)
import CSS.Property (value)
import CSS.Size (rem, px, pct, vh)
import CSS.Text (letterSpacing)
import CSS.Text.Shadow (textShadow)
import Capability.Navigate (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Type.Proxy (Proxy(..))

type State iInput = { iInput :: iInput }

data Action iInput iOutput
  = Input iInput
  | Output iOutput
  | Logoff

type Slots iQuery iOutput = ( inner :: H.Slot iQuery iOutput Unit )

_inner = Proxy :: Proxy "inner"

component
  :: forall iQuery iInput iOutput m
  . MonadAff m
  => Navigate m Route
  => H.Component iQuery iInput iOutput m
  -> H.Component iQuery iInput iOutput m
component innerComponent = H.mkComponent
  { initialState: \iInput -> { iInput }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , receive = Just <<< Input

    }
  } where
    handleAction
      :: Action iInput iOutput
      -> H.HalogenM (State iInput) (Action iInput iOutput)
          (Slots iQuery iOutput) iOutput m Unit
    handleAction = case _ of
      Input input -> H.modify_ _ { iInput = input }
      Output output -> H.raise output
      Logoff -> navigate Route.Logoff

    handleQuery
      :: forall a
      . iQuery a
      -> H.HalogenM (State iInput) (Action iInput iOutput)
          (Slots iQuery iOutput) iOutput m (Maybe a)
    handleQuery = H.query _inner unit


    render
      :: State iInput
      -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
    render { iInput } =
      HH.div [
        HC.style do
           paddingTop $ rem 5.0
      ]
      [ HH.header [
           HC.style do
              position fixed
              CSS.top $ px 0.0
              width $ pct 100.0
              height $ rem 4.0
              boxShadow (px 10.0) (px 10.0) (px 20.0) (rgba 0 0 24 0.75)
              display flex
              alignItems center
              justifyContent flexStart
              backgroundColor themeColor
              zIndex 1
           ]
        [ HH.div [
             HC.style do
                paddingLeft $ rem 1.0
                color paperColor
                display flex
                flexDirection row
                alignItems center
                flexBasis $ px 0.0
                flexGrow 1.0
                flexShrink 1.0
             ]
          [ HH.img [
               HC.style do
                  width $ px 40.0
               , HP.src bookCover -- COMPILER ERROR!!
               ]
          , HH.span [
               HC.style do
                  paddingLeft $ rem 1.0
                  fontSize $ rem 1.3
                  themeFont
                  textShadow (px 0.0) (px 0.0) (px 5.0) (rgba 0 0 0 1.0)
                  fontWeight $ FontWeight $ value "500"
                  letterSpacing $ rem 0.05
               ]
            [ HH.text "Functional Programming Made Easier" ]
          , HH.div [
               HC.style do
                  display flex
                  flexGrow 1.0
                  justifyContent flexEnd
                  paddingRight $ rem 1.0
               ]
            [ HH.span [
                 HC.style do
                    themeFont
                    fontWeight $ FontWeight $ value "500"
                    color white
                    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
                    cursor pointer
                 , HE.onClick $ const Logoff
                 ] [ HH.text "Logoff" ]
            ]
          ]
        ]
      , HH.div [
           HC.style do
              display flex
              alignItems center
              justifyContent center
              minHeight $ vh 90.0
           ]
        [ HH.slot _inner unit innerComponent iInput Output ]
      ]
