module Page.Demo exposing (..)

import Api.FlightTask exposing (FlightTask)
import Api.TaskProgress exposing (ProgressPoint)
import Common.FlightTaskUtils exposing (taskToMap3dItems, taskToMapItems)
import Common.GeoUtils exposing (metersElevation)
import Common.Utils exposing (roundN)
import Dict exposing (Dict)
import Element exposing (Element, column, row, spacing, text)
import Map3dUtils exposing (Map3dItem)
import MapUtils exposing (MapItem(..))


type alias Model =
    { flightTask : FlightTask
    , points : Dict String ProgressPoint
    }


init : FlightTask -> Model
init flightTask =
    { flightTask = flightTask
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
    in
    taskToMapItems model.flightTask ++ pointItems


map3dItems : Model -> List Map3dItem
map3dItems model =
    let
        pointItems =
            model.points
                |> Dict.toList
                |> List.map (\( id, p ) -> Map3dUtils.Marker id ( p.lat, p.lon ) p.altitude)

        taskItems =
            model.flightTask
                |> taskToMap3dItems
    in
    taskItems ++ pointItems


withPointUpdate : String -> ProgressPoint -> Model -> Model
withPointUpdate id point model =
    { model | points = Dict.insert id point model.points }


type Msg
    = ProgressUpdated String ProgressPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgressUpdated id point ->
            ( model |> withPointUpdate id point
            , Cmd.none
            )


type alias ProgressPointStats =
    { id : String
    , speed : Float
    , distance : Float
    , target : String
    }


view : Model -> Element Msg
view model =
    let
        toStats : String -> ProgressPoint -> Maybe ProgressPointStats
        toStats id point =
            Maybe.map2
                (\s t ->
                    { id = id
                    , speed = roundN 2 (s * 3.6)
                    , distance = roundN 2 (point.distance / 1000)
                    , target = t
                    }
                )
                point.speed
                point.target

        leaderboard =
            model.points
                |> Dict.toList
                |> List.filterMap (\( id, point ) -> toStats id point)
                |> List.sortBy (\stats -> -stats.speed)
                |> List.map
                    (\stats ->
                        row
                            [ spacing 10 ]
                            [ text stats.id
                            , text <| String.fromFloat stats.speed ++ " km/h"
                            , text <| String.fromFloat stats.distance ++ " km"
                            , text stats.target
                            ]
                    )
    in
    column [ spacing 10 ]
        leaderboard
