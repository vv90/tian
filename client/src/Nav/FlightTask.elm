module Nav.FlightTask exposing (..)

import Geo.GeoUtils exposing (addDeg, bearing, bearingFromDeg, destination, getBearing, linePerpendicularToBearing)
import List.Extra as ListX
import MapUtils exposing (MapItem(..))
import Nav.NavPoint exposing (NavPoint)
import Nav.Units exposing (..)
import Parser exposing (DeadEnd)



-- type TaskItem = TaskItem NavPoint


type Turnpoint
    = Cylinder Meters


type TaskStart
    = StartLine Meters
    | StartCylinder Meters


type TaskFinish
    = FinishLine Meters
    | FinishCylinder Meters


type alias FlightTask =
    { start : ( NavPoint, TaskStart )
    , turnpoints : List ( NavPoint, Turnpoint )
    , finish : ( NavPoint, TaskFinish )
    }



-- toGeoPoint : Turnpoint -> GeoPoint
-- toGeoPoint tp =
--   case tp of
--     Cylinder _ np -> { lon = np.lon, lat = np.lat }
-- startLine : FlightTask -> (GeoPoint, GeoPoint)
-- startLine task =


makeLegLines : NavPoint -> NavPoint -> List ( NavPoint, Turnpoint ) -> List MapItem
makeLegLines startPoint finishPoint turnpoints =
    List.foldr
        (\( np, tp ) ( prev, lines ) -> ( np, turnpointToMapItem ( np, tp ) :: Line [ prev.geoPoint, np.geoPoint ] :: lines ))
        ( finishPoint, [] )
        turnpoints
        |> (\( prev, lines ) -> Line [ prev.geoPoint, startPoint.geoPoint ] :: lines)


makePerpendicularLine : Meters -> NavPoint -> NavPoint -> MapItem
makePerpendicularLine lineLength origin target =
    let
        originGeo =
            origin.geoPoint

        targetGeo =
            target.geoPoint

        nextPointBearing =
            bearing originGeo targetGeo

        ( lp1, lp2 ) =
            linePerpendicularToBearing lineLength originGeo nextPointBearing
    in
    Line [ lp1, lp2 ]


startToMapItem : NavPoint -> ( NavPoint, TaskStart ) -> MapItem
startToMapItem nextPoint ( np, start ) =
    case start of
        StartLine r ->
            makePerpendicularLine r np nextPoint

        StartCylinder r ->
            Circle np.geoPoint r


finishToMapItem : NavPoint -> ( NavPoint, TaskFinish ) -> MapItem
finishToMapItem prevPoint ( np, finish ) =
    case finish of
        FinishLine r ->
            makePerpendicularLine r np prevPoint

        FinishCylinder r ->
            Circle np.geoPoint r


turnpointToMapItem : ( NavPoint, Turnpoint ) -> MapItem
turnpointToMapItem ( np, tp ) =
    case tp of
        Cylinder r ->
            Circle np.geoPoint r


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
    List.foldr
        -- at each step take the current turnpoint (`(np, tp)`) and add the corresponding circle for it and a line from the previous nav point (`prev`)
        (\( np, tp ) ( prev, lines ) -> ( np, turnpointToMapItem ( np, tp ) :: Line [ prev.geoPoint, np.geoPoint ] :: lines ))
        -- initialize with the finish nav point and a map item for the finish
        ( Tuple.first task.finish, [ finishToMapItem (lastNavPointBeforeFinish task) task.finish ] )
        task.turnpoints
        -- add a map item for the start and a line to it from the first nav point (`firstAfterStart`)
        |> (\( prev, lines ) -> startToMapItem (firstNavPointAfterStart task) task.start :: Line [ prev.geoPoint, (Tuple.first >> (\x -> x.geoPoint)) task.start ] :: lines)
