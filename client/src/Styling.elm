module Styling exposing (buttonDefault, buttonDisabled)

import Common.Palette as Palette
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


buttonBase : List (Attribute msg)
buttonBase =
    [ padding 15
    , Border.rounded 5
    ]


buttonDefault : List (Attribute msg)
buttonDefault =
    buttonBase
        ++ [ Background.color Palette.primary
           , Font.color Palette.primaryFont
           ]


buttonDisabled : List (Attribute msg)
buttonDisabled =
    buttonBase
        ++ [ Background.color Palette.lightGray
           , Font.color Palette.white
           ]
