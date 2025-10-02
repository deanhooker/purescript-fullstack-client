module AppTheme where

import Prelude

import CSS (CSS)
import CSS.Color (Color, rgb)
import CSS.Font (sansSerif, fontFamily)
import Data.NonEmpty ((:|))

paperColor :: Color
paperColor = rgb 0xd9 0xd9 0xd9

themeColor :: Color
themeColor = rgb 0x00 0x66 0x75

themeFont :: CSS
themeFont = fontFamily [ "Verdana" ] $ sansSerif :| []
