{-# LANGUAGE NoFieldSelectors #-}
module TaskProgressUtils where

import Relude
import NavPoint (NavPoint)
import qualified NavPoint
import qualified FlightTrack
import qualified FlightTask
import FlightTrack (FlightTrack (FlightTrack))
import TrackPoint (TrackPoint (TrackPoint))
import qualified TrackPoint
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Polygon as Polygon
import qualified Data.Geo.Jord.Angle as Angle
import Data.Time (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import FlightTask (FlightTask, TaskStart(..), Turnpoint(..), TaskFinish(..))
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Models (S84)
import Geo (Latitude (..), Longitude (..), Elevation, Distance (..), GeoPosition (..), GeoPosition3d(..), RecordedGeoPosition(..))
import Geo.Utils (s84position, perpendicular)
import ProgressPoint (ProgressPoint (ProgressPoint))
import qualified ProgressPoint
import TaskProgress (TaskProgress (TaskProgress))
import qualified TaskProgress
import Entity (Entity (..))
import Data.List.NonEmpty (cons)
import TimeUtils (diffTimeToHours, diffTimeToSeconds)


distanceToTarget :: GeoPosition a =>  NavPoint -> a -> Distance
distanceToTarget np tp =
    let
        dis =
            GreatCircle.distance
                (s84position np)
                (s84position tp)
    in
        DistanceMeters $ Length.toMetres dis


-- toProgressPoint :: Maybe NavPoint -> Distance -> Maybe Double -> TrackPoint -> ProgressPoint
-- toProgressPoint target distance speed tp =
--     ProgressPoint
--         { time =  TrackPoint.time tp
--         , lat = TrackPoint.lat tp
--         , lon = TrackPoint.lon tp
--         , altitude = TrackPoint.altitudeGps tp
--         , target = (\t -> (t, distanceToTarget t tp)) <$> target
--         , distance = distance
--         , speed = speed
--         }

targetNavPoint :: (NavPoint, TaskFinish) -> [(NavPoint, Turnpoint)] -> NavPoint
targetNavPoint finish tps =
    fromMaybe
        (fst finish)
        (fst <$> viaNonEmpty head tps)

-- for debugging
taskStartLine :: FlightTask -> ((Latitude, Longitude), (Latitude, Longitude))
taskStartLine ft =
    let
        (startNp, StartLine startRadius) = FlightTask.start ft
        targetNp = targetNavPoint (FlightTask.finish ft) (FlightTask.turnpoints ft)

        targetPosition =
            s84position targetNp

        startPosition =
            s84position startNp

        startBearing =
            GreatCircle.initialBearing
                startPosition
                targetPosition

        startLine sb =
            perpendicular startPosition sb (Length.metres startRadius)

        arc = startBearing >>= startLine

        toCoords pos =
            ( LatitudeDegrees $ Geodetic.decimalLatitude pos
            , LongitudeDegrees $ Geodetic.decimalLongitude pos
            )
        res =
            (\p ->
                ( (toCoords . GreatCircle.minorArcStart) p
                , (toCoords . GreatCircle.minorArcEnd) p
                )
            ) <$> arc

        fallback =
            ( ( LatitudeDegrees 0, LongitudeDegrees 0)
            , ( LatitudeDegrees 0, LongitudeDegrees 0)
            )
    in
        fromMaybe fallback res

data TpCrossing = TpCrossing
    { time :: DiffTime
    , lat :: Latitude
    , lon :: Longitude
    , altitude :: Elevation
    }

instance GeoPosition TpCrossing where
    latitude x = x.lat
    longitude x = x.lon

startLineCrossed :: (GeoPosition a, GeoPosition3d b, RecordedGeoPosition b) => FlightTask -> a -> b -> Maybe TpCrossing
startLineCrossed ft lastPos currTp =
    let
        (startNp, StartLine startRadius) = FlightTask.start ft

        targetNp = targetNavPoint (FlightTask.finish ft) (FlightTask.turnpoints ft)

        targetPosition =
            s84position targetNp

        startPosition =
            s84position startNp

        lastPosition =
            s84position lastPos
        currPosition =
            s84position currTp

        startBearing =
            GreatCircle.initialBearing
                startPosition
                targetPosition

        startLine sb =
            perpendicular startPosition sb (Length.metres startRadius)

        trackLine =
            GreatCircle.minorArc lastPosition currPosition

        startLineSide line p =
            GreatCircle.side
                p (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)

        checkCrossingDirection line target p1 p2 =
            case
                ( GreatCircle.side target (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)
                , GreatCircle.side p1 (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)
                , GreatCircle.side p2 (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)
                )
            of
                (GreatCircle.LeftOf, p, GreatCircle.LeftOf) | p /= GreatCircle.LeftOf -> True
                (GreatCircle.RightOf, p, GreatCircle.RightOf) | p /= GreatCircle.RightOf -> True
                _ -> False



        startLineCrossing sl tl =
            mfilter
                (\_ -> checkCrossingDirection sl targetPosition (GreatCircle.minorArcStart tl) (GreatCircle.minorArcEnd tl))
                (GreatCircle.intersection sl tl)

        toResult pos =
            ( LatitudeDegrees $ Angle.toDecimalDegrees $ Geodetic.latitude pos
            , LongitudeDegrees $ Angle.toDecimalDegrees $ Geodetic.longitude pos
            )

        distance = distanceToTarget startNp currTp
    in
        startLineCrossing
        <$> (startBearing >>= startLine)
        <*> trackLine
        >>= fmap
                (const
                    TpCrossing
                        { time = time currTp
                        , lat = latitude currTp
                        , lon = longitude currTp
                        , altitude = altitude currTp
                        }
                )--toProgressPoint (Just targetNp) distance Nothing currTp)


turnpointCrossed :: (GeoPosition3d a, RecordedGeoPosition a) => NavPoint -> a -> (NavPoint, Turnpoint) -> Maybe TpCrossing
turnpointCrossed targetNp currTp (np, Cylinder radius) =
    let
        (DistanceMeters npDistance) = distanceToTarget np currTp
    in
        if npDistance <= radius
        then
            -- Just $ toProgressPoint (Just targetNp) currTp
            Just $ TpCrossing 
                { time = time currTp
                , lat = latitude currTp
                , lon = longitude currTp
                , altitude = altitude currTp
                }
        else
            Nothing

finishCrossed :: GeoPosition a => (NavPoint, TaskFinish) -> NavPoint -> a -> TrackPoint -> Maybe TpCrossing
finishCrossed (np, FinishCylinder radius) prevNp lastPos currTp =
    let
        (DistanceMeters npDistance) = distanceToTarget np currTp
    in
        if npDistance <= radius
        then
            -- Just $ toProgressPoint Nothing currTp
            Just $ TpCrossing 
                { time = TrackPoint.time currTp
                , lat = latitude currTp
                , lon = longitude currTp
                , altitude = TrackPoint.altitudeGps currTp
                }
        else
            Nothing

finishCrossed (np, FinishLine radius) prevNp lastPos currTp =
    let
        reversedFinishBearing =
            GreatCircle.initialBearing
                (s84position np)
                (s84position prevNp)
        finishPosition =
            s84position np

        lastPosition =
            s84position lastPos

        currPosition =
            s84position currTp

        trackLine =
            GreatCircle.minorArc lastPosition currPosition

        finishLine fb =
           GreatCircle.minorArc
                ( GreatCircle.destination
                    finishPosition
                    (Angle.subtract (Angle.decimalDegrees 90) fb)
                    (Length.metres radius)
                )
                ( GreatCircle.destination
                    finishPosition
                    (Angle.add (Angle.decimalDegrees 90) fb)
                    (Length.metres radius)
                )

        finishCrossed =
            GreatCircle.intersection
                <$> (reversedFinishBearing >>= finishLine)
                <*> trackLine
                -- >>= fmap (\_ -> toProgressPoint Nothing currTp)
                >>= fmap 
                    ( const $ TpCrossing 
                        { time = TrackPoint.time currTp
                        , lat = latitude currTp
                        , lon = longitude currTp
                        , altitude = TrackPoint.altitudeGps currTp
                        }
                    )
    in
    Nothing

-- type TaskState = ([(NavPoint, Turnpoint)], NonEmpty ProgressPoint)

data TaskState = TaskState
    { progressPoints :: NonEmpty ProgressPoint
    , unfinishedTps :: [(NavPoint, Turnpoint)]
    , finishedTps :: [HorizontalPosition S84]
    , startTime :: Maybe DiffTime
    , finishedLegsDistance :: Length.Length
    -- , distance :: Length.Length
    }

progressInit :: (GeoPosition3d a, RecordedGeoPosition a) => FlightTask -> a -> TaskState
progressInit ft tp =
    let
        (startNp, _) = FlightTask.start ft
        -- progressPoint =
            -- toProgressPoint (Just startNp) (DistanceMeters 0) Nothing tp
        progressPoint = 
            ProgressPoint 
                { time = time tp
                , lat = latitude tp
                , lon = longitude tp
                , altitude = altitude tp
                , target = Just startNp
                , distance = DistanceMeters 0
                , speed = Nothing
                }
    in
    -- ( FlightTask.turnpoints ft
    -- , progressPoint :| []
    -- )
    TaskState
        { progressPoints = progressPoint :| []
        , unfinishedTps = FlightTask.turnpoints ft
        , finishedTps = []
        , startTime = Nothing
        , finishedLegsDistance = Length.zero
        -- , distance = Length.zero
        }

progressAdvance :: (GeoPosition3d a, RecordedGeoPosition a) => FlightTask -> TaskState -> a -> TaskState
progressAdvance ft prevState tp =
    let
        lastPos = head $ prevState.progressPoints
        -- unfinishedTurnpoints = unfinishedTps prevState
        started = startLineCrossed ft lastPos tp

        checkTurnpoint :: ((NavPoint, Turnpoint), [(NavPoint, Turnpoint)]) -> Maybe (TpCrossing, NavPoint, [(NavPoint, Turnpoint)])
        checkTurnpoint (currTurnpoint, remainingTurnpoints) =
            (, fst currTurnpoint, remainingTurnpoints)
            <$> turnpointCrossed
                    (targetNavPoint (FlightTask.finish ft) remainingTurnpoints)
                    tp
                    currTurnpoint

        tpCrossed =
            uncons prevState.unfinishedTps
            >>= checkTurnpoint

        sumFinishedLegsDistance (tp : tps) =
            snd
            $ foldl'
                (\(b, d) a -> (a, Length.add (GreatCircle.distance a b) d))
                (tp, Length.zero)
                tps
        sumFinishedLegsDistance [] =
            Length.zero
        
        calcSpeed dist time =
            (\st -> dist / diffTimeToSeconds (time - st) ) <$> prevState.startTime
        -- currentLegDistance lastTp nextTp = 
        --     GreatCircle.minorArc (lastTpPos prevState) (s84position $ target lastPos)
    in
    case (started, tpCrossed) of
        (Just p, _) ->
            TaskState
                { progressPoints = 
                    ProgressPoint 
                        { time = p.time
                        , lat = p.lat
                        , lon = p.lon
                        , altitude = p.altitude
                        , target = Just $ targetNavPoint (FlightTask.finish ft) (prevState.unfinishedTps)
                        , distance = DistanceMeters 0
                        , speed = Nothing
                        } `cons` prevState.progressPoints
                         --cons p (progressPoints prevState)
                , unfinishedTps = FlightTask.turnpoints ft
                , finishedTps = [s84position $ fst $ FlightTask.start ft]
                , startTime = Just p.time
                , finishedLegsDistance = Length.zero
                }
            -- (FlightTask.turnpoints ft, cons p progressPoints)

        (Nothing, Just (p, currTp, restTps)) ->
            -- (restTps, cons p progressPoints)
            let
                fl = s84position currTp : prevState.finishedTps
                flDist = sumFinishedLegsDistance fl
                flDistMeters = Length.toMetres flDist
            in
            TaskState
                { progressPoints = 
                    ProgressPoint
                        { time = p.time
                        , lat = p.lat
                        , lon = p.lon
                        , altitude = p.altitude
                        , target = Just $ targetNavPoint (FlightTask.finish ft) restTps
                        , distance = DistanceMeters flDistMeters
                        , speed = calcSpeed flDistMeters (p.time) --Just (flDist / diffTimeToHours (startTime prevState))
                        } `cons` prevState.progressPoints
                    --cons p (progressPoints prevState)
                , unfinishedTps = restTps
                , finishedTps = fl
                , startTime = prevState.startTime
                , finishedLegsDistance = flDist
                }

        (Nothing, Nothing) ->
            -- ( unfinishedTurnpoints
            -- , toProgressPoint
            --     (fmap fst $ target $ head progressPoints)
            --     tp
            --   `cons` progressPoints
            -- )
            let
                tgt =
                    (\(ProgressPoint _ _ _ _ t _ _) -> t) $ head $ prevState.progressPoints
                leg =
                    join
                    $ GreatCircle.minorArc -- returns Nothing if previous turnpoint position is equal to target position
                    <$> viaNonEmpty head (prevState.finishedTps) -- or if there is no previous position
                    <*> fmap s84position tgt -- or if there is no target

                legDist =
                    GreatCircle.alongTrackDistance (s84position tp) <$> leg
                currDist = 
                    Length.toMetres
                    $ Length.add
                        (prevState.finishedLegsDistance)
                        (fromMaybe Length.zero legDist)
            in
            TaskState
                { progressPoints =
                    ProgressPoint 
                        { time = time tp
                        , lat = latitude tp
                        , lon = longitude tp
                        , altitude = altitude tp
                        , target = tgt
                        , distance = DistanceMeters currDist
                        , speed = calcSpeed currDist (time tp)
                        } `cons` prevState.progressPoints
                    -- toProgressPoint tgt tp
                    -- `cons` progressPoints prevState
                , unfinishedTps = prevState.unfinishedTps
                , finishedTps = prevState.finishedTps
                , startTime = prevState.startTime
                , finishedLegsDistance = prevState.finishedLegsDistance
                -- , distance =
                --     Length.add
                --         (finishedLegsDistance prevState)
                --         (fromMaybe Length.zero legDist)
                }

progress :: Entity Int32 FlightTask -> FlightTrack -> TaskProgress
progress (Entity taskId ft) (FlightTrack date compId points) =
    let
        p :| ps = points
        state = foldl' (progressAdvance ft) (progressInit ft p) ps
    in
    TaskProgress
        taskId
        date
        compId
        (reverse $ toList state.progressPoints)
        state.finishedTps
