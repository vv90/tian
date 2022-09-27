module Components.Select exposing (..)

import Element exposing (Element, below, column, el, htmlAttribute, text)
import Element.Events exposing (onClick)
import Element.Input as Input
import Html.Events exposing (onBlur)


type alias Model a =
    { selected : Maybe a
    , options : List a
    , isOpen : Bool
    }


init : List a -> Maybe a -> Model a
init options defaultValue =
    { selected = defaultValue
    , options = options
    , isOpen = False
    }


type Msg a
    = ValueSelected a
    | Clicked
    | Blurred


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        ValueSelected value ->
            ( { model | selected = Just value, isOpen = False }, Cmd.none )

        Clicked ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        Blurred ->
            ( { model | isOpen = False }, Cmd.none )


view : (a -> String) -> String -> Model a -> Element (Msg a)
view toLabel placeholder model =
    let
        label =
            case model.selected of
                Just value ->
                    toLabel value

                Nothing ->
                    placeholder

        options =
            if model.isOpen then
                column []
                    (List.map (\x -> el [ onClick (ValueSelected x) ] <| text <| toLabel x) model.options)

            else
                Element.none
    in
    el []
        (Input.button
            [ htmlAttribute <| onBlur <| Blurred
            , below options
            ]
            { label = text label
            , onPress = Just Clicked
            }
        )
