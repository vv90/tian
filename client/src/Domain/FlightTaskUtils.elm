module Domain.FlightTaskUtils exposing
    ( finishToMap3dItem
    , firstNavPointAfterStart
    , lastNavPointBeforeFinish
    , navPoints
    , startToMap3dItem
    , taskToMap3dItems
    , turnpointToMap3dItem
    )

import Api.Types exposing (..)
import Domain.GeoUtils exposing (bearing, linePerpendicularToBearing)
import List.Extra as ListX
import Map3dUtils as M3d exposing (Map3dItem)


startToMap3dItem : NavPoint -> ( NavPoint, TaskStart ) -> Map3dItem
startToMap3dItem nextPoint ( np, start ) =
    case start of
        StartLine r ->
            let
                startBearing =
                    bearing { lat = np.lat, lon = np.lon } { lat = nextPoint.lat, lon = nextPoint.lon }

                ( lp1, lp2 ) =
                    linePerpendicularToBearing (DistanceMeters r) { lat = np.lat, lon = np.lon } startBearing
            in
            -- makePerpendicularLine r np nextPoint
            -- Circle { lat = np.lat, lon = np.lon } (DistanceMeters r)
            M3d.Line [ ( lp1, ElevationMeters 0 ), ( lp2, ElevationMeters 0 ) ]


finishToMap3dItem : NavPoint -> ( NavPoint, TaskFinish ) -> Map3dItem
finishToMap3dItem prevPoint ( np, finish ) =
    case finish of
        FinishLine r ->
            -- makePerpendicularLine r np prevPoint
            M3d.Cylinder { lat = np.lat, lon = np.lon } (DistanceMeters r) (ElevationMeters 10000)

        FinishCylinder r ->
            M3d.Cylinder { lat = np.lat, lon = np.lon } (DistanceMeters r) (ElevationMeters 10000)


turnpointToMap3dItem : ( NavPoint, Turnpoint ) -> Map3dItem
turnpointToMap3dItem ( np, tp ) =
    case tp of
        TurnpointCylinder r ->
            M3d.Cylinder { lat = np.lat, lon = np.lon } (DistanceMeters r) (ElevationMeters 10000)


firstNavPointAfterStart : FlightTask -> NavPoint
firstNavPointAfterStart task =
    task.turnpoints
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault (Tuple.first task.finish)


lastNavPointBeforeFinish : FlightTask -> NavPoint
lastNavPointBeforeFinish task =
    task.turnpoints
        |> ListX.last
        |> Maybe.map Tuple.first
        |> Maybe.withDefault (Tuple.first task.start)


taskToMap3dItems : FlightTask -> List Map3dItem
taskToMap3dItems task =
    List.foldl
        -- at each step take the current turnpoint (`(np, tp)`) and add the corresponding circle for it and a line from the previous nav point (`prev`)
        (\( np, tp ) ( prev, lines ) ->
            ( np
            , turnpointToMap3dItem ( np, tp )
                :: M3d.Line [ ( { lat = prev.lat, lon = prev.lon }, ElevationMeters 0 ), ( { lat = np.lat, lon = np.lon }, ElevationMeters 0 ) ]
                :: lines
            )
        )
        -- initialize with the start nav point and a map item for the start
        ( Tuple.first task.start
        , [ startToMap3dItem (firstNavPointAfterStart task) task.start ]
        )
        task.turnpoints
        -- add a map item for the finish and a line to it from the last nav point (`lastBeforeFinish`)
        |> (\( prev, lines ) ->
                finishToMap3dItem (lastNavPointBeforeFinish task) task.finish
                    :: M3d.Line
                        [ ( { lat = prev.lat, lon = prev.lon }, ElevationMeters 0 )
                        , (Tuple.first >> (\x -> ( { lat = x.lat, lon = x.lon }, ElevationMeters 0 ))) task.finish
                        ]
                    :: lines
           )


navPoints : FlightTask -> List NavPoint
navPoints task =
    let
        start =
            Tuple.first task.start

        finish =
            Tuple.first task.finish

        turnpoints =
            List.map Tuple.first task.turnpoints
    in
    start :: turnpoints ++ [ finish ]
