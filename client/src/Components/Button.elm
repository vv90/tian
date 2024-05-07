module Components.Button exposing (..)

import Common.Palette as Palette
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


primaryButton : List (Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
primaryButton attrs { label, onPress } =
    Input.button
        ([ Font.color Palette.white
         , Background.color Palette.primary
         , padding 10
         , Border.rounded 5
         ]
            ++ attrs
        )
        { onPress = onPress, label = label }
