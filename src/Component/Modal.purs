module Component.Modal where

import Prelude hiding (top)

import AppTheme (paperColor, themeColor, themeFont)
import CSS.Background (backgroundColor)
import CSS.Color (gray, rgba, white)
import CSS.Common (center)
import CSS.Cursor (cursor, notAllowed)
import CSS.Display (display, fixed, flex, position, zIndex)
import CSS.Flexbox (alignItems, column, flexDirection, flexEnd, justifyContent, row)
import CSS.Font (FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry (height, left, marginLeft, maxWidth, minHeight, minWidth, padding, top, width)
import CSS.Overflow (overflow, overflowAuto)
import CSS.Property (value)
import CSS.Size (Size(..), pct, rem)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State iInput =
  { iInput :: iInput
  , affirmativeDisabled :: Boolean
  , negativeDisabled :: Boolean
  }

data Output iOutput
  = Affirmative
  | Negative
  | InnerOutput iOutput

data InnerOutput iOutput
  = PassThrough iOutput
  | EnableAffirmative
  | DisableAffirmative
  | EnableNegative
  | DisableNegative
  | CloseAffirmative
  | CloseNegative

data Action iInput iOutput
  = Input iInput
  | Output (InnerOutput iOutput)
  | AffirmativeClicked
  | NegativeClicked

type Slots iQuery iOutput = ( inner :: H.Slot iQuery (InnerOutput iOutput) Unit )

_inner = Proxy :: Proxy "inner"

type Config =
  { affirmativeLabel :: String
  , negativeLabel :: String
  , displayButtons :: ButtonDisplay
  , affirmativeDisabled :: Boolean
  , negativeDisabled :: Boolean
  }

data ButtonDisplay
  = DisplayBothButtons
  | DisplayAffirmative
  | DisplayNegative
  | DisplayNoButtons

defaultConfig :: Config
defaultConfig =
  { affirmativeLabel: "OK"
  , negativeLabel: "CANCEL"
  , displayButtons: DisplayBothButtons
  , affirmativeDisabled: false
  , negativeDisabled: false
  }

component :: forall iQuery iInput iOutput m
          . MonadAff m
          => Config
          -> H.Component iQuery iInput (InnerOutput iOutput) m
          -> H.Component iQuery iInput (Output iOutput) m
component config innerComponent = H.mkComponent
  { initialState: \iInput ->
     { iInput
     , affirmativeDisabled: config.affirmativeDisabled
     , negativeDisabled: config.negativeDisabled
     }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Input
      }
  }
  where
    handleAction :: (Action iInput iOutput)
                 -> H.HalogenM (State iInput) (Action iInput iOutput)
                    (Slots iQuery iOutput) (Output iOutput) m Unit
    handleAction = case _ of
      Input input -> H.modify_ _ { iInput = input }
      Output innerOutput -> case innerOutput of
        PassThrough output -> H.raise $ InnerOutput output
        EnableAffirmative -> H.modify_ _ { affirmativeDisabled = false }
        DisableAffirmative -> H.modify_ _ { affirmativeDisabled = true }
        EnableNegative -> H.modify_ _ { negativeDisabled = false }
        DisableNegative -> H.modify_ _ { negativeDisabled = true }
        CloseAffirmative -> handleAction AffirmativeClicked
        CloseNegative -> handleAction NegativeClicked
      AffirmativeClicked -> H.raise Affirmative
      NegativeClicked -> H.raise Negative

    handleQuery
      :: forall a
      . iQuery a
      -> H.HalogenM (State iInput) (Action iInput iOutput)
          (Slots iQuery iOutput) (Output iOutput) m (Maybe a)
    handleQuery = H.query _inner unit

    render :: State iInput
           -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
    render { iInput, affirmativeDisabled, negativeDisabled } =
      let { affirmativeLabel
          , negativeLabel
          , displayButtons
          } = config
          displayAffirmative = case displayButtons of
            DisplayAffirmative -> true
            DisplayBothButtons -> true
            _ -> false
          displayNegative = case displayButtons of
            DisplayNegative -> true
            DisplayBothButtons -> true
            _ -> false in
      HH.div [
        HC.style do
           display flex
           alignItems center
           justifyContent center
           position fixed
           top $ Size $ value 0.0
           left $ Size $ value 0.0
           width (pct 100.0)
           height (pct 100.0)
           overflow overflowAuto
           backgroundColor (rgba 0 0 0 0.4)
           zIndex 1
      ]
      [ HH.div [
          HC.style do
             display flex
             flexDirection column
             padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
             backgroundColor paperColor
             minWidth (rem 40.0)
             maxWidth (pct 80.0)
             minHeight (rem 10.0)
        ]
        [ HH.div [
            HC.style do
               display flex
               flexDirection column
               padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
          ]
          [ HH.slot _inner unit innerComponent iInput Output ]
        , HH.div [
            HC.style do
               display flex
               flexDirection row
               alignItems center
               justifyContent flexEnd
               width (pct 100.0)
               backgroundColor paperColor
          ]
          [ if not displayAffirmative then HH.text "" else
            HH.button
            [
              buttonStyle affirmativeDisabled
            , HP.disabled affirmativeDisabled
            , HE.onClick $ const AffirmativeClicked
            ]
            [ HH.text affirmativeLabel ]
          , if not displayNegative then HH.text "" else
            HH.button
            [
              buttonStyle negativeDisabled
            , HP.disabled negativeDisabled
            , HE.onClick $ const NegativeClicked
            ]
            [ HH.text negativeLabel ]
          ]
        ]]
      where
        buttonStyle isDisabled = HC.style do
          themeFont
          fontWeight $ FontWeight $ value "500"
          color if isDisabled then gray else white
          when isDisabled $ cursor notAllowed
          padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
          backgroundColor themeColor
          width (rem 8.0)
          marginLeft (rem 2.0)
          fontSize (rem 0.9)
