
module TaskProgressUtils where

import Relude
import NavPoint (NavPoint)
import qualified NavPoint
import qualified FlightTrack
import qualified FlightTask
import FlightTrack (TrackPoint, FlightTrack (FlightTrack))
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Polygon as Polygon
import qualified Data.Geo.Jord.Angle as Angle
import Data.Time (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import FlightTask (FlightTask, TaskStart(..), Turnpoint(..), TaskFinish(..))
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Models (S84)
import Geo (Latitude (..), Longitude (..), Elevation, Distance (..), GeoPosition (..))
import Geo.Utils (s84position, perpendicular)
import ProgressPoint (ProgressPoint (..))
import TaskProgress (TaskProgress (..))
import Entity (Entity (..))


distanceToTarget :: NavPoint -> TrackPoint -> Distance
distanceToTarget np tp =
    let 
        dis = 
            GreatCircle.distance 
                (s84position np)
                (s84position tp)
    in
        DistanceMeters $ Length.toMetres dis

diffTimeToMillis :: DiffTime -> Int
diffTimeToMillis t =
    let
        picos = diffTimeToPicoseconds t
        millis = picos `div` 1000000000
    in
        fromIntegral millis

millisToDiffTime :: Int -> DiffTime
millisToDiffTime t =
    let
        picos = fromIntegral t * 1000000000
    in
        picosecondsToDiffTime picos

toProgressPoint :: Maybe NavPoint -> TrackPoint -> ProgressPoint
toProgressPoint target tp =
    ProgressPoint
        { time =  diffTimeToMillis $ FlightTrack.time tp
        , lat = FlightTrack.lat tp
        , lon = FlightTrack.lon tp
        , altitude = FlightTrack.altitudeGps tp
        , target = (\t -> (t, distanceToTarget t tp)) <$> target
        }

targetNavPoint :: (NavPoint, TaskFinish) -> [(NavPoint, Turnpoint)] -> NavPoint
targetNavPoint finish tps =
    fromMaybe 
        (fst finish)
        (fst <$> viaNonEmpty head tps)

startLineCrossed :: GeoPosition a => FlightTask -> a -> TrackPoint -> Maybe ProgressPoint--(Latitude, Longitude)
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
    in
        startLineCrossing 
        <$> (startBearing >>= startLine)
        <*> trackLine 
        >>= fmap (\_ -> toProgressPoint (Just targetNp) currTp)

turnpointCrossed :: NavPoint -> TrackPoint -> (NavPoint, Turnpoint) -> Maybe ProgressPoint
turnpointCrossed targetNp currTp (np, Cylinder radius) =
    let
        (DistanceMeters npDistance) = distanceToTarget np currTp
    in
        if npDistance <= radius
        then
            Just $ toProgressPoint (Just targetNp) currTp
        else
            Nothing

finishCrossed :: GeoPosition a => (NavPoint, TaskFinish) -> NavPoint -> a -> TrackPoint -> Maybe ProgressPoint
finishCrossed (np, FinishCylinder radius) prevNp lastPos currTp =
    let
        (DistanceMeters npDistance) = distanceToTarget np currTp
    in
        if npDistance <= radius
        then
            Just $ toProgressPoint Nothing currTp
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
                >>= fmap (\_ -> toProgressPoint Nothing currTp)
    in
    Nothing

type TaskState = ([(NavPoint, Turnpoint)], NonEmpty ProgressPoint)

progressInit :: FlightTask -> TrackPoint -> TaskState
progressInit ft tp =
    let
        (startNp, _) = FlightTask.start ft
        progressPoint = 
            toProgressPoint (Just startNp) tp
            
    in
    ( FlightTask.turnpoints ft
    , progressPoint :| []
    )

progressAdvance :: FlightTask -> TaskState -> TrackPoint -> TaskState
progressAdvance ft (unfinishedTurnpoints, progressPoints) tp =
    let
        lastPos = head progressPoints
        started = startLineCrossed ft lastPos tp

        checkTurnpoint :: ((NavPoint, Turnpoint), [(NavPoint, Turnpoint)]) -> Maybe (ProgressPoint, [(NavPoint, Turnpoint)])
        checkTurnpoint (currTurnpoint, remainingTurnpoints) = 
            (, remainingTurnpoints)
            <$> turnpointCrossed 
                    (targetNavPoint (FlightTask.finish ft) remainingTurnpoints) 
                    tp 
                    currTurnpoint

        tpCrossed = 
            uncons unfinishedTurnpoints 
            >>= checkTurnpoint
        
        
    in
    case (started, tpCrossed) of
        (Just p, _) -> 
            (FlightTask.turnpoints ft, p :| [])
            
        (Nothing, Just (p, restTps)) ->
            (restTps, p :| toList progressPoints)

        (Nothing, Nothing) ->
            ( unfinishedTurnpoints
            , toProgressPoint 
                (fmap fst $ target $ head progressPoints)
                tp 
              :| toList progressPoints
            )
            
progress :: Entity Int32 FlightTask -> FlightTrack -> TaskProgress
progress (Entity taskId ft) (FlightTrack date compId points) = 
    let 
        p :| ps = points
        (_, progressPoints) = foldl' (progressAdvance ft) (progressInit ft p) ps
    in
    TaskProgress 
        taskId
        date
        compId
        (toList progressPoints)