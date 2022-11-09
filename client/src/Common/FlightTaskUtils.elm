module Common.FlightTaskUtils exposing (..)

import Api.FlightTask exposing (FlightTask, TaskFinish(..), TaskStart(..), Turnpoint(..))
import Api.Geo exposing (Distance(..))
import Api.NavPoint exposing (NavPoint)
import List.Extra as ListX
import MapUtils exposing (LineStyle(..), MapItem(..))


startToMapItem : NavPoint -> ( NavPoint, TaskStart ) -> MapItem
startToMapItem nextPoint ( np, start ) =
    case start of
        StartLine r ->
            -- makePerpendicularLine r np nextPoint
            Circle ( np.lat, np.lon ) (DistanceMeters r)


finishToMapItem : NavPoint -> ( NavPoint, TaskFinish ) -> MapItem
finishToMapItem prevPoint ( np, finish ) =
    case finish of
        FinishLine r ->
            -- makePerpendicularLine r np prevPoint
            Circle ( np.lat, np.lon ) (DistanceMeters r)

        FinishCylinder r ->
            Circle ( np.lat, np.lon ) (DistanceMeters r)


turnpointToMapItem : ( NavPoint, Turnpoint ) -> MapItem
turnpointToMapItem ( np, tp ) =
    case tp of
        Cylinder r ->
            Circle ( np.lat, np.lon ) (DistanceMeters r)


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


taskToMapItems : FlightTask -> List MapItem
taskToMapItems task =
    List.foldl
        -- at each step take the current turnpoint (`(np, tp)`) and add the corresponding circle for it and a line from the previous nav point (`prev`)
        (\( np, tp ) ( prev, lines ) ->
            ( np
            , turnpointToMapItem ( np, tp )
                :: Line TaskLine [ ( prev.lat, prev.lon ), ( np.lat, np.lon ) ]
                :: lines
            )
        )
        -- initialize with the start nav point and a map item for the start
        ( Tuple.first task.start
        , [ startToMapItem (firstNavPointAfterStart task) task.start ]
        )
        task.turnpoints
        -- add a map item for the finish and a line to it from the last nav point (`lastBeforeFinish`)
        |> (\( prev, lines ) ->
                finishToMapItem (lastNavPointBeforeFinish task) task.finish
                    :: Line
                        TaskLine
                        [ ( prev.lat, prev.lon )
                        , (Tuple.first >> (\x -> ( x.lat, x.lon ))) task.finish
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
