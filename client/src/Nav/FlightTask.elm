module Nav.FlightTask exposing (..)

import Nav.Units exposing (..)
import Geo.GeoUtils exposing (GeoPoint, bearing, destination, addDeg)
import Nav.NavPoint exposing (NavPoint, navPointToGeoPoint)
import MapUtils exposing (MapItem(..))
import List.Extra as ListX


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
  { start: (NavPoint, TaskStart)
  , turnpoints: List (NavPoint, Turnpoint)
  , finish: (NavPoint, TaskFinish)
  }

-- toGeoPoint : Turnpoint -> GeoPoint
-- toGeoPoint tp =
--   case tp of
--     Cylinder _ np -> { lon = np.lon, lat = np.lat }



makeLegLines : NavPoint -> NavPoint -> List (NavPoint, Turnpoint) -> List MapItem
makeLegLines startPoint finishPoint turnpoints = 
  List.foldr 
    (\(np, tp) (prev, lines) -> (np, turnpointToMapItem (np, tp) :: (Line [navPointToGeoPoint prev, navPointToGeoPoint np]) :: lines)) 
    (finishPoint, []) 
    turnpoints
  |> (\(prev, lines) -> (Line [navPointToGeoPoint prev, navPointToGeoPoint startPoint]) :: lines)

makePerpendicularLine : Meters -> NavPoint -> NavPoint -> MapItem
makePerpendicularLine radius origin target =
  let
    originGeo = navPointToGeoPoint origin
    targetGeo = navPointToGeoPoint target
    nextPointBearing = bearing originGeo targetGeo
    lp1 = destination (addDeg (Deg 90) nextPointBearing) radius originGeo
    lp2 = destination (addDeg (Deg -90) nextPointBearing) radius originGeo
  in
    Line [lp1, lp2]

startToMapItem : NavPoint -> (NavPoint, TaskStart) -> MapItem
startToMapItem nextPoint (np, start) = 
  case start of
    StartLine r -> 
      makePerpendicularLine r np nextPoint
    StartCylinder r ->
      Circle (navPointToGeoPoint np) r

finishToMapItem : NavPoint -> (NavPoint, TaskFinish) -> MapItem
finishToMapItem prevPoint (np, finish) =
  case finish of
    FinishLine r ->
      makePerpendicularLine r np prevPoint
    FinishCylinder r ->
      Circle (navPointToGeoPoint np) r

turnpointToMapItem : (NavPoint, Turnpoint) -> MapItem
turnpointToMapItem (np, tp) =
  case tp of
    Cylinder r -> Circle (navPointToGeoPoint np) r

taskToMapItems : FlightTask -> List MapItem
taskToMapItems task =
  let
    firstAfterStart = 
      task.turnpoints 
      |> List.head 
      |> Maybe.map Tuple.first 
      |> Maybe.withDefault (Tuple.first task.finish) 
    
    lastBeforFinish =
      task.turnpoints
      |> ListX.last
      |> Maybe.map Tuple.first 
      |> Maybe.withDefault (Tuple.first task.start) 

    
  in
    List.foldr 
      -- at each step take the current turnpoint (`(np, tp)`) and add the corresponding circle for it and a line from the previous nav point (`prev`)
      (\(np, tp) (prev, lines) -> (np, turnpointToMapItem (np, tp) :: (Line [navPointToGeoPoint prev, navPointToGeoPoint np]) :: lines)) 
      -- initialize with the finish nav point and a map item for the finish
      (Tuple.first task.finish, [finishToMapItem lastBeforFinish task.finish]) 
      task.turnpoints
    -- add a map item for the start and a line to it from the first nav point (`firstAfterStart`)
    |> (\(prev, lines) -> (startToMapItem firstAfterStart task.start) :: (Line [navPointToGeoPoint prev, (Tuple.first >> navPointToGeoPoint) task.start]) :: lines)