
module TaskProgress where

import Relude
import NavPoint (NavPoint)
import qualified NavPoint
import qualified FlightTrack
import qualified FlightTask
import FlightTrack (TrackPoint)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Polygon as Polygon
import qualified Data.Geo.Jord.Angle as Angle
import Data.Time (DiffTime)
import FlightTask (FlightTask, TaskStart(..), Turnpoint(..), TaskFinish(..))
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Models (S84)
import Geo (Latitude (..), Longitude (..), Elevation, Distance (..), GeoPosition (..))
import Geo.Utils (s84position, perpendicular)


data ProgressPoint = ProgressPoint
    { time :: DiffTime
    , lat :: Latitude
    , lon :: Longitude
    , altitude :: Elevation
    , target :: Maybe (NavPoint, Distance)
    -- , distanceToTarget :: NavPoint.Length
    -- , distanceCovered :: NavPoint.Length
    }

instance GeoPosition ProgressPoint where
    latitude = lat
    longitude = lon

distanceToTarget :: NavPoint -> TrackPoint -> Distance
distanceToTarget np tp =
    let
        (LongitudeDegrees xLon) = NavPoint.lon np
        (LatitudeDegrees xLat) = NavPoint.lat np
        xPos = Geodetic.s84Pos xLat xLon

        (LongitudeDegrees yLon) = FlightTrack.lon tp
        (LatitudeDegrees yLat) = FlightTrack.lat tp
        yPos = Geodetic.s84Pos yLat yLon

        dis = GreatCircle.distance xPos yPos
    in
        DistanceMeters $ Length.toMetres dis

toProgressPoint :: Maybe NavPoint -> TrackPoint -> ProgressPoint
toProgressPoint target tp =
    ProgressPoint
        { time = FlightTrack.time tp
        , lat = FlightTrack.lat tp
        , lon = FlightTrack.lon tp
        , altitude = FlightTrack.altitudeGps tp
        , target = (\t -> (t, distanceToTarget t tp)) <$> target
        }

-- data TaskProgress = TaskProgress ProgressPoint [TrackPoint -> ProgressPoint]

-- type Milestone = (NavPoint, (TrackPoint, TrackPoint) -> Maybe ProgressPoint)

-- milestones :: FlightTask -> [Milestone]
-- milestones ft =
--     let 
--         start@(startNp, _) = FlightTask.start ft
--         finish@(finishNp, _) = FlightTask.finish ft
    
--         lastNp = 
--             fromMaybe
--                 startNp
--                 (fst <$> viaNonEmpty last (FlightTask.turnpoints ft))

--         startMilestone = 
--             startLineCrossed ft

--         finishMilestone = 
--             finishCrossed finish lastNp
               
--         turnpointMilestones res tps =
--             case tps of
--                 [] -> 
--                     res ++ [(finishNp, finishMilestone)]
--                 [curr@(currNp, _)] -> 
--                     turnpointMilestones (res ++ [(currNp, turnpointCrossed curr finishNp)]) []
--                 curr@(currNp, _) : next@(nextNp, _) : rest -> 
--                     turnpointMilestones (res ++ [(currNp, turnpointCrossed curr nextNp)]) (next : rest)      
--     in
--     turnpointMilestones [(startNp, startLineCrossed ft)] (FlightTask.turnpoints ft)

progressStart :: FlightTask -> TrackPoint -> ProgressPoint
progressStart ft tp =
    let
        (startNp, _) = FlightTask.start ft
    in
     ProgressPoint
        (FlightTrack.time tp)
        (FlightTrack.lat tp)
        (FlightTrack.lon tp)
        (FlightTrack.altitudeGps tp)
        (Just (startNp, distanceToTarget startNp tp))
        -- (distanceToTurnpoint startNp tp)
    -- , startNp
    -- )

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
            -- fromMaybe
            --     (fst $ FlightTask.finish ft)  
            --     (fst <$> viaNonEmpty head (FlightTask.turnpoints ft))

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
            
        -- toProgressPoint :: HorizontalPosition S84 -> ProgressPoint
        -- toProgressPoint _ = 
        --     ProgressPoint 
        --         (FlightTrack.time currTp)
        --         (FlightTrack.lat currTp)
        --         (FlightTrack.lon currTp)
        --         (FlightTrack.altitudeGps currTp)
        --         (Just (targetNp, distanceToTarget targetNp currTp))
                
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
            Just $ ProgressPoint
                (FlightTrack.time currTp)
                (FlightTrack.lat currTp)
                (FlightTrack.lon currTp)
                (FlightTrack.altitudeGps currTp)
                (Just (targetNp, distanceToTarget targetNp currTp))
        else
            Nothing

finishCrossed :: GeoPosition a => (NavPoint, TaskFinish) -> NavPoint -> a -> TrackPoint -> Maybe ProgressPoint
finishCrossed (np, FinishCylinder radius) prevNp lastPos currTp =
    let
        (DistanceMeters npDistance) = distanceToTarget np currTp
    in
        if npDistance <= radius
        then 
            Just $ ProgressPoint
                (FlightTrack.time currTp)
                (FlightTrack.lat currTp)
                (FlightTrack.lon currTp)
                (FlightTrack.altitudeGps currTp)
                Nothing
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
        
        -- toProgressPoint _ =
        --     ProgressPoint 
        --         (FlightTrack.time currTp)
        --         (FlightTrack.lat currTp)
        --         (FlightTrack.lon currTp)
        --         (FlightTrack.altitudeGps currTp)
        --         Nothing

        finishCrossed = 
            GreatCircle.intersection 
                <$> (reversedFinishBearing >>= finishLine)
                <*> trackLine
                >>= fmap (\_ -> toProgressPoint Nothing currTp)
    in
    Nothing

-- advance :: FlightTask -> [(NavPoint, Turnpoint)] -> TrackPoint -> [ProgressPoint]
-- advance ft tps pts@(currTp, prevTp) =
--     let
--         taskStart = startLineCrossed ft pts
--     in
--     case taskStart of
--         Just s -> advance ft (turnpoints ft) 

type TaskProgress = ([(NavPoint, Turnpoint)], NonEmpty ProgressPoint)

progress :: FlightTask -> TaskProgress -> TrackPoint -> TaskProgress
progress ft (unfinishedTurnpoints, progressPoints) tp =
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
                (Just $ targetNavPoint (FlightTask.finish ft) unfinishedTurnpoints) 
                tp 
              :| toList progressPoints
            )
            
