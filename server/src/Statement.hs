{-# LANGUAGE QuasiQuotes #-}

module Statement where

import Control.Arrow (left)
import Data.Profunctor (Profunctor (lmap))
import Data.Vector (Vector)
import Data.Vector as V (foldl, fromList, indexed)
import Entity (Entity (..))
import FlightTask (FlightTask (..), TaskFinish (..), TaskStart (..), Turnpoint (..))
import Geo (Direction (..), Distance (..), Elevation (..), Latitude (..), Longitude (..), degreesDirection, degreesLatitude, degreesLongitude, metersDistance, metersElevation)
import Hasql.Statement (Statement, refineResult)
import Hasql.TH (rowsAffectedStatement, singletonStatement, vectorStatement)
import NavPoint (NavPoint (..))
import Relude

newtype NavPointId = NavPointId Int32

decodeNavPoint :: (Text, Text, Maybe Text, Double, Double, Double, Text, Maybe Int32, Maybe Double, Maybe Text, Text) -> Either Text NavPoint
decodeNavPoint (name, code, country, lat, lon, elev, style, rwdir, rwlen, freq, descr) = do
  wpStyle <- left (\e -> "Failed to parse waypoint style for waypoint " <> code <> ": " <> e) $ readEither $ toString style

  pure
    $ NavPoint
      name
      code
      country
      (LatitudeDegrees lat)
      (LongitudeDegrees lon)
      (ElevationMeters elev)
      wpStyle
      (DirectionDegrees <$> rwdir)
      (DistanceMeters <$> rwlen)
      freq
      descr

getNavPointsStatement :: Statement () (Vector NavPoint)
getNavPointsStatement =
  refineResult
    (traverse decodeNavPoint)
    [vectorStatement|
            select 
                name :: text, 
                code :: text, 
                country :: text?, 
                lat :: float8,
                lon :: float8,
                elev :: float8,
                style :: text,
                rwdir :: int4?,
                rwlen :: float8?,
                freq :: text?,
                descr :: text
            from nav_points
        |]

getNpStatement :: Statement Text (Vector NavPoint)
getNpStatement =
  refineResult
    (traverse decodeNavPoint)
    [vectorStatement|
            select 
                name :: text, 
                code :: text, 
                country :: text?, 
                lat :: float8,
                lon :: float8,
                elev :: float8,
                style :: text,
                rwdir :: int4?,
                rwlen :: float8?,
                freq :: text?,
                descr :: text
            from nav_points
            where name = $1 :: Text
        |]

deleteDuplicateNavPointsStatement :: Statement (Vector Text) Int64
deleteDuplicateNavPointsStatement =
  [rowsAffectedStatement|
        delete from nav_points where code = any($1 :: text[])
    |]

saveNavPointsStatement :: Statement (Vector NavPoint) Int64
saveNavPointsStatement =
  lmap
    nest
    [rowsAffectedStatement|
            insert into nav_points (name, code, country, lat, lon, elev, style, rwdir, rwlen, freq, descr) 
            select * from unnest ( $1 :: text[], $2 :: text[], $3 :: text?[], $4 :: float8[], $5 :: float8[], $6 :: float8[], $7 :: text[], $8 :: int4?[], $9 :: float8?[], $10 :: text?[], $11 :: text[] )
        |]
  where
    nest nps =
      let names = toText . name <$> nps
          codes = toText . code <$> nps
          countries = fmap toText . country <$> nps
          latitudes = degreesLatitude . lat <$> nps
          longitudes = degreesLongitude . lon <$> nps
          elevations = metersElevation . elev <$> nps
          styles = show . style <$> nps
          rwdirs = fmap degreesDirection . rwdir <$> nps
          rwlens = fmap metersDistance . rwlen <$> nps
          freqs = fmap toText . freq <$> nps
          descs = toText . desc <$> nps
       in (names, codes, countries, latitudes, longitudes, elevations, styles, rwdirs, rwlens, freqs, descs)

encodeStartType :: TaskStart -> Text
encodeStartType = \case
  StartLine _ -> "StartLine"

decodeStartType :: Text -> Either Text (Double -> TaskStart)
decodeStartType = \case
  "StartLine" -> Right StartLine
  x -> Left $ "Failed to parse start type: unrecognized value '" <> x <> "'"

encodeTurnpointType :: Turnpoint -> Text
encodeTurnpointType = \case
  Cylinder _ -> "Cylinder"

decodeTurnpointType :: Text -> Either Text (Double -> Turnpoint)
decodeTurnpointType = \case
  "Cylinder" -> Right Cylinder
  x -> Left $ "Failed to parse turnpoint type: unrecognized value '" <> x <> "'"

encodeFinishType :: TaskFinish -> Text
encodeFinishType = \case
  FinishLine _ -> "FinishLine"
  FinishCylinder _ -> "FinishCylinder"

decodeFinishType :: Text -> Either Text (Double -> TaskFinish)
decodeFinishType = \case
  "FinishLine" -> Right FinishLine
  "FinishCylinder" -> Right FinishCylinder
  x -> Left $ "Failed to parse finish type: unrecognized value '" <> x <> "'"

decodeFlightTaskRow ::
  ( Int32, -- id
    Text, -- start_type
    Double, -- start_radius
    Text, -- start_point_name
    Text, -- start_point_code
    Maybe Text, -- start_point_country
    Double, -- start_point_lat
    Double, -- start_point_lon
    Double, -- start_point_elev
    Text, -- start_point_style
    Maybe Int32, -- start_point_rwdir
    Maybe Double, -- start_point_rwlen
    Maybe Text, -- start_point_freq
    Text, -- start_point_descr
    Text, -- finish_type
    Double, -- finish_radius
    Text, -- finish_point_name
    Text, -- finish_point_code
    Maybe Text, -- finish_point_country
    Double, -- finish_point_lat
    Double, -- finish_point_lon
    Double, -- finish_point_elev
    Text, -- finish_point_style
    Maybe Int32, -- finish_point_rwdir
    Maybe Double, -- finish_point_rwlen
    Maybe Text, -- finish_point_freq
    Text, -- finish_point_descr
    Text, -- turnpoint_type
    Double, -- turnpoint_radius
    Int32, -- turnpoint_order
    Text, -- turnpoint_point_name
    Text, -- turnpoint_point_code
    Maybe Text, -- turnpoint_point_country
    Double, -- turnpoint_point_lat
    Double, -- turnpoint_point_lon
    Double, -- turnpoint_point_elev
    Text, -- turnpoint_point_style
    Maybe Int32, -- turnpoint_point_rwdir
    Maybe Double, -- turnpoint_point_rwlen
    Maybe Text, -- turnpoint_point_freq
    Text -- turnpoint_point_descr
  ) ->
  Either Text (Entity Int32 FlightTask)
decodeFlightTaskRow
  ( eid,
    start_type,
    start_radius,
    start_point_name,
    start_point_code,
    start_point_country,
    start_point_lat,
    start_point_lon,
    start_point_elev,
    start_point_style,
    start_point_rwdir,
    start_point_rwlen,
    start_point_freq,
    start_point_descr,
    finish_type,
    finish_radius,
    finish_point_name,
    finish_point_code,
    finish_point_country,
    finish_point_lat,
    finish_point_lon,
    finish_point_elev,
    finish_point_style,
    finish_point_rwdir,
    finish_point_rwlen,
    finish_point_freq,
    finish_point_descr,
    turnpoint_type,
    turnpoint_radius,
    _turnpoint_order,
    turnpoint_name,
    turnpoint_code,
    turnpoint_country,
    turnpoint_lat,
    turnpoint_lon,
    turnpoint_elev,
    turnpoint_style,
    turnpoint_rwdir,
    turnpoint_rwlen,
    turnpoint_freq,
    turnpoint_descr
    ) =
    let startPoint =
          decodeNavPoint
            ( start_point_name,
              start_point_code,
              start_point_country,
              start_point_lat,
              start_point_lon,
              start_point_elev,
              start_point_style,
              start_point_rwdir,
              start_point_rwlen,
              start_point_freq,
              start_point_descr
            )
        finishPoint =
          decodeNavPoint
            ( finish_point_name,
              finish_point_code,
              finish_point_country,
              finish_point_lat,
              finish_point_lon,
              finish_point_elev,
              finish_point_style,
              finish_point_rwdir,
              finish_point_rwlen,
              finish_point_freq,
              finish_point_descr
            )
        turnPoint =
          decodeNavPoint
            ( turnpoint_name,
              turnpoint_code,
              turnpoint_country,
              turnpoint_lat,
              turnpoint_lon,
              turnpoint_elev,
              turnpoint_style,
              turnpoint_rwdir,
              turnpoint_rwlen,
              turnpoint_freq,
              turnpoint_descr
            )

        start = (,) <$> startPoint <*> (decodeStartType start_type <*> pure start_radius)
        finish = (,) <$> finishPoint <*> (decodeFinishType finish_type <*> pure finish_radius)
        turn = (,) <$> turnPoint <*> (decodeTurnpointType turnpoint_type <*> pure turnpoint_radius)
        flightTask = FlightTask <$> start <*> (one <$> turn) <*> finish
     in Entity eid <$> flightTask

combineFlightTaskRows :: Vector (Entity Int32 FlightTask) -> [Entity Int32 FlightTask]
combineFlightTaskRows rows =
  -- here we rely on the fact that the rows are sorted by task id and turnpoint order
  let combine :: [Entity Int32 FlightTask] -> Entity Int32 FlightTask -> [Entity Int32 FlightTask]
      combine allEntities currentEntity@(Entity currentKey (FlightTask _ currentTurnpoints _)) =
        case allEntities of
          -- when previous row has the same key as current row, combine the turnpoints
          (Entity lastKey (FlightTask start turnpoints finish)) : rest
            | lastKey == currentKey ->
                Entity lastKey (FlightTask start (turnpoints <> currentTurnpoints) finish) : rest
          -- in all other cases just add the current row to the list
          _ ->
            currentEntity : allEntities
   in V.foldl combine [] rows

getFlightTaskStatement :: Statement Int32 (Maybe (Entity Int32 FlightTask))
getFlightTaskStatement =
  refineResult
    (fmap (viaNonEmpty head . combineFlightTaskRows) . traverse decodeFlightTaskRow)
    -- (\x -> traverse decodeFlightTaskRow x)
    [vectorStatement|
            select 
                id :: int4,
                start_type :: text,
                start_radius :: float8,
                start_point_name :: text,
                start_points.code :: text,
                start_points.country :: text?,
                start_points.lat :: float8,
                start_points.lon :: float8,
                start_points.elev :: float8,
                start_points.style :: text,
                start_points.rwdir :: int4?,
                start_points.rwlen :: float8?,
                start_points.freq :: text?,
                start_points.descr :: text,

                finish_type :: text,
                finish_radius :: float8,
                finish_point_name :: text,
                finish_points.code :: text,
                finish_points.country :: text?,
                finish_points.lat :: float8,
                finish_points.lon :: float8,
                finish_points.elev :: float8,
                finish_points.style :: text,
                finish_points.rwdir :: int4?,
                finish_points.rwlen :: float8?,
                finish_points.freq :: text?,
                finish_points.descr :: text,

                turnpoint_type :: text,
                turnpoint_radius :: float8,
                turnpoint_number :: int4,
                turnpoint_name :: text,
                turn_points.code :: text,
                turn_points.country :: text?,
                turn_points.lat :: float8,
                turn_points.lon :: float8,
                turn_points.elev :: float8,
                turn_points.style :: text,
                turn_points.rwdir :: int4?,
                turn_points.rwlen :: float8?,
                turn_points.freq :: text?,
                turn_points.descr :: text

            from tasks
            join task_turnpoints on tasks.id = task_turnpoints.task_id
            join nav_points as start_points on tasks.start_point_name = start_points.name
            join nav_points as finish_points on tasks.finish_point_name = finish_points.name
            join nav_points as turn_points on task_turnpoints.turnpoint_name = turn_points.name
            where id = $1 :: int4
            order by id asc, turnpoint_number asc
        |]

getAllFlightTasksStatement :: Statement () [Entity Int32 FlightTask]
getAllFlightTasksStatement =
  refineResult
    (fmap combineFlightTaskRows . traverse decodeFlightTaskRow)
    [vectorStatement|
            select 
                id :: int4,
                start_type :: text,
                start_radius :: float8,
                start_point_name :: text,
                start_points.code :: text,
                start_points.country :: text?,
                start_points.lat :: float8,
                start_points.lon :: float8,
                start_points.elev :: float8,
                start_points.style :: text,
                start_points.rwdir :: int4?,
                start_points.rwlen :: float8?,
                start_points.freq :: text?,
                start_points.descr :: text,

                finish_type :: text,
                finish_radius :: float8,
                finish_point_name :: text,
                finish_points.code :: text,
                finish_points.country :: text?,
                finish_points.lat :: float8,
                finish_points.lon :: float8,
                finish_points.elev :: float8,
                finish_points.style :: text,
                finish_points.rwdir :: int4?,
                finish_points.rwlen :: float8?,
                finish_points.freq :: text?,
                finish_points.descr :: text,

                turnpoint_type :: text,
                turnpoint_radius :: float8,
                turnpoint_number :: int4,
                turnpoint_name :: text,
                turn_points.code :: text,
                turn_points.country :: text?,
                turn_points.lat :: float8,
                turn_points.lon :: float8,
                turn_points.elev :: float8,
                turn_points.style :: text,
                turn_points.rwdir :: int4?,
                turn_points.rwlen :: float8?,
                turn_points.freq :: text?,
                turn_points.descr :: text

            from tasks
            join task_turnpoints on tasks.id = task_turnpoints.task_id
            join nav_points as start_points on tasks.start_point_name = start_points.name
            join nav_points as finish_points on tasks.finish_point_name = finish_points.name
            join nav_points as turn_points on task_turnpoints.turnpoint_name = turn_points.name
            order by id asc, turnpoint_number asc
        |]

saveFlightTaskStatement :: Statement FlightTask Int32
saveFlightTaskStatement =
  lmap
    ( \(FlightTask (startPoint, start) _ (finishPoint, finish)) ->
        ( name startPoint,
          startType start,
          startRadius start,
          name finishPoint,
          finishType finish,
          finishRadius finish
        )
    )
    [singletonStatement|
            
            insert into tasks (start_point_name, start_type, start_radius, finish_point_name, finish_type, finish_radius)
            values ($1 :: text, $2 :: text, $3 :: float8, $4 :: text, $5 :: text, $6 :: float8)
            returning id :: int4
        |]
  where
    startType :: TaskStart -> Text
    startType = \case
      StartLine _ -> "StartLine"
    startRadius = \case
      StartLine r -> r

    finishType :: TaskFinish -> Text
    finishType = \case
      FinishLine _ -> "FinishLine"
      FinishCylinder _ -> "FinishCylinder"
    finishRadius = \case
      FinishLine r -> r
      FinishCylinder r -> r

saveFlightTaskTurnpointsStatement :: Statement (Entity Int32 FlightTask) Int64
saveFlightTaskTurnpointsStatement =
  lmap
    ( \(Entity key (FlightTask _ turnpoints _)) ->
        let tpsv = (V.indexed . V.fromList) turnpoints
         in ( key <$ tpsv,
              name . fst . snd <$> tpsv,
              turnpointType . snd . snd <$> tpsv,
              turnpointRadius . snd . snd <$> tpsv,
              fromIntegral . fst <$> tpsv
            )
    )
    [rowsAffectedStatement|
            insert into task_turnpoints (task_id, turnpoint_name, turnpoint_type, turnpoint_radius, turnpoint_number)
            select * from unnest ($1 :: int4[], $2 :: text[], $3 :: text[], $4 :: float8[], $5 :: int4[])
        |]
  where
    turnpointType = \case
      Cylinder _ -> "Cylinder" :: Text
    turnpointRadius = \case
      Cylinder r -> r
