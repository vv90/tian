module Styling exposing (buttonBase, buttonDefault, buttonDisabled, shadowDefault)

import Common.Palette as Palette
import Element exposing (Attribute, Color, Element, focused, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


shadowDefault : Attribute msg
shadowDefault =
    Border.shadow
        { offset = ( 2, 2 )
        , size = 0
        , blur = 5
        , color = Palette.shadow
        }


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
