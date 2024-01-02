module Page.Demo exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    , withPointUpdate
    )

import Api.Types exposing (..)
import Common.ApiResult exposing (ApiResult, DeferredResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredIsPending, deferredToMaybe)
import Common.JsonCodecsExtra exposing (tupleDecoder)
import Common.Palette as Palette
import Common.Utils exposing (roundN)
import Dict exposing (Dict)
import Domain.FlightTaskUtils exposing (taskToMap3dItems, taskToMapItems)
import Domain.GeoUtils exposing (metersElevation)
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
    { demoData : DeferredResult ( FlightTask, List NameMatch )
    , points : Dict String ProgressPoint
    }


init : Model
init =
    { demoData = NotStarted
    , points = Dict.empty
    }


mapItems : Model -> List MapItem
mapItems model =
    let
        toMarker : ( String, { a | lat : Latitude, lon : Longitude, altitude : Elevation } ) -> MapItem
        toMarker ( id, p ) =
            Marker
                { lat = p.lat, lon = p.lon }
                (id ++ " " ++ (p.altitude |> metersElevation |> roundN 2 |> String.fromFloat) ++ "m")

        pointItems : List MapItem
        pointItems =
            model.points
                |> Dict.toList
                |> List.map toMarker

        taskItems : List MapItem
        taskItems =
            model.demoData
                |> (deferredToMaybe >> Maybe.andThen Result.toMaybe)
                |> MaybeX.unwrap [] (Tuple.first >> taskToMapItems)
    in
    taskItems ++ pointItems


map3dItems : Model -> List Map3dItem
map3dItems model =
    let
        pointItems : List Map3dItem
        pointItems =
            model.points
                |> Dict.toList
                |> List.map (\( id, p ) -> Map3dUtils.Marker id { lat = p.lat, lon = p.lon } p.altitude)

        taskItems : List Map3dItem
        taskItems =
            model.demoData
                |> (deferredToMaybe >> Maybe.andThen Result.toMaybe)
                |> MaybeX.unwrap [] (Tuple.first >> taskToMap3dItems)
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

        DemoInit (Finished res) ->
            ( { model | demoData = Resolved res }
            , Ports.startDemo ()
            )

        MessageReceived str ->
            let
                upd : Result D.Error ( String, ProgressPoint )
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
        Resolved (Ok _) ->
            Ports.messageReceiver MessageReceived

        _ ->
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
        findName : String -> Maybe String
        findName id =
            model.demoData
                |> (deferredToMaybe >> Maybe.andThen Result.toMaybe)
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
        leaderboard : Element msg
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

        greeting : Element Msg
        greeting =
            let
                item : String -> Element msg
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
                    (text "Competition Tracking")
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
                , if deferredIsPending model.demoData then
                    Input.button
                        Styling.buttonDisabled
                        { onPress = Nothing
                        , label = text "Starting demo..."
                        }

                  else
                    Input.button
                        Styling.buttonDefault
                        { onPress = Just (DemoInit Started)
                        , label = text "Start Demo"
                        }
                ]
    in
    case deferredToMaybe model.demoData of
        Nothing ->
            greeting

        Just _ ->
            leaderboard
