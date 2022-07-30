module IntersectionDemo exposing (..)

import Geo.GeoUtils exposing (GeoPoint, lineIntersection)
import Html exposing (Html, div, text)
import Maybe.Extra as MaybeX exposing (isNothing)
import PointDemo exposing (showPoint)


type alias Model =
    { p1 : Maybe GeoPoint
    , p2 : Maybe GeoPoint
    , p3 : Maybe GeoPoint
    , p4 : Maybe GeoPoint
    , intersectionPoint : Maybe GeoPoint
    }


init : () -> Model
init () =
    { p1 = Nothing
    , p2 = Nothing
    , p3 = Nothing
    , p4 = Nothing
    , intersectionPoint = Nothing
    }


selectPoint : GeoPoint -> Model -> Model
selectPoint p model =
    if isNothing model.p1 then
        { model | p1 = Just p }

    else if isNothing model.p2 then
        { model | p2 = Just p }

    else if isNothing model.p3 then
        { model | p3 = Just p }

    else if isNothing model.p4 then
        { model | p4 = Just p }

    else
        model


selectIntersection : Model -> Model
selectIntersection model =
    { model
        | intersectionPoint =
            MaybeX.andThen4
                (\p1 p2 p3 p4 -> lineIntersection ( p1, p2 ) ( p3, p4 ))
                model.p1
                model.p2
                model.p3
                model.p4
    }


type Msg
    = PointSelected GeoPoint
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PointSelected p ->
            ( (selectPoint p >> selectIntersection) model, Cmd.none )

        Reset ->
            ( { model | p1 = Nothing, p2 = Nothing, p3 = Nothing, p4 = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ viewPoint model.p1
        , viewPoint model.p2
        , viewPoint model.p3
        , viewPoint model.p4
        , viewPoint model.intersectionPoint
        ]


viewPoint : Maybe GeoPoint -> Html Msg
viewPoint mp =
    case mp of
        Just p ->
            div [] [ (showPoint >> text) p ]

        Nothing ->
            div [] [ text "--" ]
