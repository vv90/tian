module Page.Demo exposing (..)

import Api.Demo exposing (NameMatch, nameMatchDecoder)
import Api.FlightTask exposing (FlightTask, flightTaskDecoder)
import Api.TaskProgress exposing (ProgressPoint, progressPointDecoder)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..))
import Common.FlightTaskUtils exposing (taskToMap3dItems, taskToMapItems)
import Common.GeoUtils exposing (metersElevation)
import Common.JsonCodecsExtra exposing (tupleDecoder)
import Common.Palette as Palette
import Common.Utils exposing (roundN)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, column, el, fill, onLeft, paddingXY, paragraph, row, shrink, spacing, spacingXY, table, text)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import List.Extra as ListX
import Map3dUtils exposing (Map3dItem)
import MapUtils exposing (MapItem(..))
import Maybe.Extra as MaybeX
import Ports
import Styling


type alias Model =
    { demoData : Maybe ( FlightTask, List NameMatch )
    , points : Dict String ProgressPoint
    }


init : Model
init =
    { demoData = Nothing
    , points = Dict.empty
    }


mapItems : Model -> List MapItem
mapItems model =
    let
        toMarker ( id, p ) =
            Marker
                ( p.lat, p.lon )
                (id ++ " " ++ (p.altitude |> metersElevation |> roundN 2 |> String.fromFloat) ++ "m")

        pointItems =
            model.points
                |> Dict.toList
                |> List.map toMarker

        taskItems =
            MaybeX.unwrap [] (Tuple.first >> taskToMapItems) model.demoData
    in
    taskItems ++ pointItems


map3dItems : Model -> List Map3dItem
map3dItems model =
    let
        pointItems =
            model.points
                |> Dict.toList
                |> List.map (\( id, p ) -> Map3dUtils.Marker id ( p.lat, p.lon ) p.altitude)

        taskItems =
            MaybeX.unwrap [] (Tuple.first >> taskToMap3dItems) model.demoData
    in
    taskItems ++ pointItems


withPointUpdate : String -> ProgressPoint -> Model -> Model
withPointUpdate id point model =
    { model | points = Dict.insert id point model.points }


getDemoTaskCmd : Cmd Msg
getDemoTaskCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = apiUrl "demoTask"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (Finished >> DemoInit)
                (tupleDecoder ( flightTaskDecoder, D.list nameMatchDecoder ))
        , timeout = Nothing
        , tracker = Nothing
        }


type Msg
    = ProgressUpdated String ProgressPoint
    | DemoInit (AsyncOperationStatus (ApiResult ( FlightTask, List NameMatch )))
    | MessageReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgressUpdated id point ->
            ( model |> withPointUpdate id point
            , Cmd.none
            )

        DemoInit Started ->
            ( model, getDemoTaskCmd )

        DemoInit (Finished (Ok data)) ->
            ( { model | demoData = Just data }
            , Ports.startDemo ()
            )

        DemoInit (Finished (Err _)) ->
            ( model, Cmd.none )

        MessageReceived str ->
            let
                upd =
                    D.decodeString (tupleDecoder ( D.string, progressPointDecoder )) str
            in
            case upd of
                Ok ( id, p ) ->
                    ( model |> withPointUpdate id p
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.demoData of
        Just _ ->
            Ports.messageReceiver MessageReceived

        Nothing ->
            Sub.none


type alias ProgressPointStats =
    { id : String
    , name : String
    , speed : Float
    , distance : Float
    , target : String
    }


view : Model -> Element Msg
view model =
    let
        findName id =
            model.demoData
                |> Maybe.andThen (Tuple.second >> ListX.find (\n -> n.compId == id))
                |> Maybe.map .name

        toStats : String -> ProgressPoint -> Maybe ProgressPointStats
        toStats id point =
            Maybe.map2
                (\s t ->
                    { id = id
                    , name = findName id |> Maybe.withDefault ""
                    , speed = roundN 2 (s * 3.6)
                    , distance = roundN 2 (point.distance / 1000)
                    , target = t
                    }
                )
                point.speed
                point.target

        -- leaderboard =
        --     model.points
        --         |> Dict.toList
        --         |> List.filterMap (\( id, point ) -> toStats id point)
        --         |> List.sortBy (\stats -> -stats.speed)
        --         |> List.map
        --             (\stats ->
        --                 row
        --                     [ spacing 10 ]
        --                     [ text stats.id
        --                     , text <| String.fromFloat stats.speed ++ " km/h"
        --                     , text <| String.fromFloat stats.distance ++ " km"
        --                     , text stats.target
        --                     ]
        --             )
        leaderboard =
            table
                [ spacingXY 20 10
                , Font.family [ Font.typeface "Roboto Mono" ]
                ]
                { data =
                    model.points
                        |> Dict.toList
                        |> List.filterMap (\( id, point ) -> toStats id point)
                        |> List.sortBy (\stats -> -stats.speed)
                , columns =
                    [ { header = text ""
                      , width = shrink
                      , view =
                            \stats -> text <| stats.id
                      }
                    , { header = text "Name"
                      , width = shrink
                      , view =
                            \stats -> text <| stats.name
                      }
                    , { header = el [ Font.alignRight ] <| text "Task speed"
                      , width = fill
                      , view =
                            \stats -> el [ Font.alignRight ] <| text <| String.fromFloat stats.speed ++ " km/h"
                      }
                    , { header = el [ Font.alignRight ] <| text "Distance"
                      , width = fill
                      , view =
                            \stats -> el [ Font.alignRight ] <| text <| String.fromFloat stats.distance ++ " km"
                      }

                    -- , { header = el [ Font.center ] <| text "Target"
                    --   , width = fill
                    --   , view =
                    --         \stats -> text stats.target
                    --   }
                    ]
                }

        greeting =
            let
                item txt =
                    el
                        [ paddingXY 10 0
                        ]
                        (text txt)
            in
            column
                [ spacing 20
                , Font.color Palette.darkGray
                ]
                [ el
                    [ Region.heading 1
                    , Font.center
                    , Font.size 27
                    , Font.bold
                    , Font.color Palette.darkerGray
                    ]
                    (text "Competition tracking")
                , paragraph
                    [ Region.mainContent ]
                    [ column [ spacing 10 ]
                        [ text "This is a proof of concept demonstration that includes:"
                        , item "• 3D map"
                        , item "• Position tracking"
                        , item "• Task progress"
                        , item "• Real-time scoring"
                        ]
                    ]
                , paragraph
                    [ Region.mainContent ]
                    [ text "For this demo real time tracking is replaced with pre-recorded flight tracks played back at 5x speed" ]
                , Input.button
                    Styling.buttonDefault
                    { onPress = Just (DemoInit Started)
                    , label = text "Start Demo"
                    }
                ]
    in
    case model.demoData of
        Nothing ->
            greeting

        Just _ ->
            leaderboard
