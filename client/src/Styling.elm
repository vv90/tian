module Styling exposing (..)

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


buttonDefault : List (Attribute msg)
buttonDefault =
    [ padding 15
    , Background.color Palette.primary
    , Font.color Palette.primaryFont
    , Border.rounded 5

    -- , shadowDefault
    ]
