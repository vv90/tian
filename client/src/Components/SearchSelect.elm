module Components.SearchSelect exposing (Model, Msg(..), filterMatchingSuggestions, init, update, view)

import Common.Palette
import Element exposing (Element, below, clipY, column, el, fill, height, maximum, mouseOver, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Input as Input


type alias Model =
    { value : String
    , isOpen : Bool
    }


init : Model
init =
    { value = ""
    , isOpen = False
    }


type Msg a
    = ValueChanged String
    | ItemSelected a
    | InputClicked
    | InputBlurred


update : Msg a -> Model -> ( Model, Cmd (Msg a), Maybe a )
update msg model =
    case msg of
        ValueChanged value ->
            ( { model | value = value }, Cmd.none, Nothing )

        ItemSelected item ->
            ( { model | value = "", isOpen = False }
            , Cmd.none
            , Just item
            )

        InputClicked ->
            ( { model | isOpen = True }, Cmd.none, Nothing )

        InputBlurred ->
            ( { model | isOpen = False }, Cmd.none, Nothing )



-- ItemSelected value ->
--     ( { model | value = value }, Cmd.none )


filterMatchingSuggestions : String -> (String -> a -> Bool) -> List a -> List a
filterMatchingSuggestions value matchFn suggestions =
    if String.isEmpty value then
        suggestions

    else
        List.filter
            (matchFn value)
            suggestions


view :
    { suggestions : List a
    , toLabel : a -> String
    , matchFn : String -> a -> Bool
    }
    -> Model
    -> Element (Msg a)
view { suggestions, toLabel, matchFn } model =
    let
        attrs : List (Element.Attribute (Msg a))
        attrs =
            [ below optionsList ]

        optionsList : Element (Msg a)
        optionsList =
            if model.isOpen then
                let
                    optionElement : a -> Element (Msg a)
                    optionElement =
                        \item ->
                            el
                                [ onClick (ItemSelected item)
                                , mouseOver [ Background.color Common.Palette.gray ]
                                , width fill
                                ]
                                (text <| toLabel item)
                in
                el
                    [ width fill
                    , height <| maximum 300 fill
                    , Background.color Common.Palette.white
                    , clipY
                    ]
                    (column
                        [ width fill
                        , scrollbarY
                        , spacing 5
                        ]
                        (suggestions
                            |> filterMatchingSuggestions model.value matchFn
                            |> List.map optionElement
                        )
                    )

            else
                Element.none

        input : Element (Msg a)
        input =
            Input.text
                [ onClick InputClicked

                -- , htmlAttribute <| onBlur <| selectMsg InputBlurred
                ]
                { onChange = ValueChanged
                , text = model.value
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Turnpoint")
                }
    in
    el attrs input
