module Nav.TaskProgress exposing (..)

import Geo.GeoUtils exposing (GeoPoint, bearing, bearingDifference, distance, lineIntersection, linePerpendicularToBearing)
import List.Nonempty as Nonempty
import Maybe.Extra as MaybeX
import Nav.FlightTask exposing (FlightTask, TaskFinish(..), TaskStart(..), Turnpoint(..), firstNavPointAfterStart, lastNavPointBeforFinish)
import Nav.FlightTrack exposing (FlightTrack, TrackPoint, trackPointToGeoPoint)
import Nav.NavPoint exposing (NavPoint)
import Nav.Units exposing (Meters(..), getDeg, leMeters)


type TaskStatus
    = NotStarted
    | LegInProgress ( NavPoint, Turnpoint ) (List ( NavPoint, Turnpoint ))
    | FinalGlide ( NavPoint, TaskFinish )
    | Finished


showTaskStatus : TaskStatus -> String
showTaskStatus status =
    case status of
        NotStarted ->
            "Not Started"

        LegInProgress ( np, _ ) _ ->
            np.name

        FinalGlide ( np, _ ) ->
            np.name

        Finished ->
            "Finished"


type alias TaskProgress =
    { task : FlightTask
    , status : TaskStatus
    , lastPoint : GeoPoint
    }


initTaskProgress : FlightTask -> FlightTrack -> TaskProgress
initTaskProgress flightTask flightTrack =
    { task = flightTask
    , status = NotStarted
    , lastPoint = (Nonempty.head >> trackPointToGeoPoint) flightTrack.points
    }


advanceTaskProgress : TrackPoint -> TaskProgress -> TaskProgress
advanceTaskProgress point progress =
    let
        start =
            checkStarted point progress
    in
    case ( start, progress.status ) of
        ( Just startPoint, _ ) ->
            { progress
                | status =
                    Maybe.map2
                        LegInProgress
                        (List.head progress.task.turnpoints)
                        (List.tail progress.task.turnpoints)
                        |> Maybe.withDefault (FinalGlide progress.task.finish)
                , lastPoint = startPoint
            }

        ( Nothing, LegInProgress ( nextNavPoint, turnpoint ) tailTurnpoints ) ->
            case checkNavPointReached point ( nextNavPoint, turnpoint ) of
                Just turnpointIntersection ->
                    { progress
                        | status =
                            Maybe.map2
                                LegInProgress
                                (List.head tailTurnpoints)
                                (List.tail tailTurnpoints)
                                |> Maybe.withDefault (FinalGlide progress.task.finish)
                        , lastPoint = turnpointIntersection
                    }

                Nothing ->
                    progress

        ( Nothing, FinalGlide ( finishNavPoint, taskFinish ) ) ->
            case checkFinished point ( finishNavPoint, taskFinish ) progress of
                Just finishIntersection ->
                    { progress
                        | status = Finished
                        , lastPoint = finishIntersection
                    }

                Nothing ->
                    progress

        ( Nothing, NotStarted ) ->
            { progress
                | lastPoint = trackPointToGeoPoint point
            }

        ( Nothing, Finished ) ->
            { progress
                | lastPoint = trackPointToGeoPoint point
            }


checkStarted : TrackPoint -> TaskProgress -> Maybe GeoPoint
checkStarted point progress =
    case progress.task.start of
        ( startNavPoint, StartLine radius ) ->
            let
                currentGeoPoint =
                    trackPointToGeoPoint point

                firstTaskTarget =
                    (firstNavPointAfterStart >> (\x -> x.geoPoint)) progress.task

                startGeo =
                    startNavPoint.geoPoint

                startBearing =
                    bearing startGeo firstTaskTarget

                startLine =
                    linePerpendicularToBearing radius startGeo startBearing

                startLineIntersection =
                    lineIntersection
                        ( progress.lastPoint, currentGeoPoint )
                        startLine

                checkIntersectionDirection : GeoPoint -> Bool
                checkIntersectionDirection _ =
                    bearingDifference
                        (bearing progress.lastPoint currentGeoPoint)
                        startBearing
                        |> (getDeg >> (<) 90)
            in
            startLineIntersection
                |> MaybeX.filter checkIntersectionDirection

        ( startNavPoint, StartCylinder radius ) ->
            let
                currentGeoPoint =
                    trackPointToGeoPoint point

                startGeoPoint =
                    startNavPoint.geoPoint

                isInsideStartCylinder =
                    distance startGeoPoint >> leMeters radius
            in
            if
                isInsideStartCylinder progress.lastPoint
                    && (not << isInsideStartCylinder) currentGeoPoint
            then
                Just startGeoPoint

            else
                Nothing


checkNavPointReached : TrackPoint -> ( NavPoint, Turnpoint ) -> Maybe GeoPoint
checkNavPointReached point ( targetNavPoint, turnpoint ) =
    case turnpoint of
        Cylinder radius ->
            let
                currentGeoPoint =
                    trackPointToGeoPoint point

                targetGeoPoint =
                    targetNavPoint.geoPoint

                isInsideTurnpoint =
                    distance targetGeoPoint
                        >> leMeters radius
            in
            if isInsideTurnpoint currentGeoPoint then
                Just currentGeoPoint

            else
                Nothing


checkFinished : TrackPoint -> ( NavPoint, TaskFinish ) -> TaskProgress -> Maybe GeoPoint
checkFinished point ( targetNavPoint, taskFinish ) progress =
    case taskFinish of
        FinishLine radius ->
            let
                currentGeoPoint =
                    trackPointToGeoPoint point

                prevTargetGeo =
                    (lastNavPointBeforFinish >> (\x -> x.geoPoint)) progress.task

                finishGeo =
                    targetNavPoint.geoPoint

                reversedFinishBearing =
                    bearing finishGeo prevTargetGeo

                finishLine =
                    linePerpendicularToBearing radius finishGeo reversedFinishBearing
            in
            lineIntersection
                ( currentGeoPoint, progress.lastPoint )
                finishLine

        FinishCylinder radius ->
            let
                currentGeoPoint =
                    trackPointToGeoPoint point

                targetGeoPoint =
                    targetNavPoint.geoPoint

                isInsideFinishCylinder =
                    distance targetGeoPoint
                        >> leMeters radius
            in
            if isInsideFinishCylinder currentGeoPoint then
                Just currentGeoPoint

            else
                Nothing
