{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TaskProgress where

import Relude
import NavPoint (NavPoint, Longitude(..), Latitude(..))
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

data ProgressPoint = ProgressPoint
    { time :: DiffTime
    , lat :: Latitude
    , lon :: Longitude
    , altitude :: NavPoint.Elevation
    , target :: Maybe (NavPoint, NavPoint.Length)
    -- , distanceToTarget :: NavPoint.Length
    -- , distanceCovered :: NavPoint.Length
    }

s84position :: Latitude -> Longitude -> HorizontalPosition S84
s84position (LatitudeDegrees lat) (LongitudeDegrees lon) = 
    Geodetic.s84Pos lat lon

distanceToTurnpoint :: NavPoint -> TrackPoint -> NavPoint.Length
distanceToTurnpoint np tp =
    let
        (LongitudeDegrees xLon) = NavPoint.lon np
        (LatitudeDegrees xLat) = NavPoint.lat np
        xPos = Geodetic.s84Pos xLat xLon

        (LongitudeDegrees yLon) = FlightTrack.lon tp
        (LatitudeDegrees yLat) = FlightTrack.lat tp
        yPos = Geodetic.s84Pos yLat yLon

        dis = GreatCircle.distance xPos yPos
    in
        NavPoint.LengthMeters $ Length.toMetres dis

data TaskProgress = TaskProgress ProgressPoint [TrackPoint -> ProgressPoint]

type Milestone = (NavPoint, (TrackPoint, TrackPoint) -> Maybe ProgressPoint)

milestones :: FlightTask -> [Milestone]
milestones ft =
    let 
        start@(startNp, _) = FlightTask.start ft
        finish@(finishNp, _) = FlightTask.finish ft
    
        lastNp = 
            fromMaybe
                startNp
                (fst <$> viaNonEmpty last (FlightTask.turnpoints ft))

        startMilestone = 
            startLineCrossed ft

        finishMilestone = 
            finishCrossed finish lastNp
               
        turnpointMilestones res tps =
            case tps of
                [] -> 
                    res ++ [(finishNp, finishMilestone)]
                [curr@(currNp, _)] -> 
                    turnpointMilestones (res ++ [(currNp, turnpointCrossed curr finishNp)]) []
                curr@(currNp, _) : next@(nextNp, _) : rest -> 
                    turnpointMilestones (res ++ [(currNp, turnpointCrossed curr nextNp)]) (next : rest)      
    in
    turnpointMilestones [(startNp, startLineCrossed ft)] (FlightTask.turnpoints ft)

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
        (Just (startNp, distanceToTurnpoint startNp tp))
        -- (distanceToTurnpoint startNp tp)
    -- , startNp
    -- )

startLineCrossed :: FlightTask -> (TrackPoint, TrackPoint) -> Maybe (ProgressPoint)
startLineCrossed ft (lastTp, currTp) =
    let
        (startNp, StartLine startRadius) = FlightTask.start ft

        targetNp =
            fromMaybe
                (fst $ FlightTask.finish ft)  
                (fst <$> viaNonEmpty head (FlightTask.turnpoints ft))

        targetPosition =
            s84position (NavPoint.lat targetNp) (NavPoint.lon targetNp)

        startPosition = 
            s84position (NavPoint.lat startNp) (NavPoint.lon startNp)

        lastPosition = 
            s84position (FlightTrack.lat lastTp) (FlightTrack.lon lastTp)

        currPosition =
            s84position (FlightTrack.lat currTp) (FlightTrack.lon currTp)

        startBearing = 
            GreatCircle.initialBearing 
                startPosition
                targetPosition

        startLine sb = 
            GreatCircle.minorArc
                ( GreatCircle.destination 
                    startPosition
                    (Angle.subtract (Angle.decimalDegrees 90) sb)
                    (Length.metres startRadius)
                )
                ( GreatCircle.destination
                    startPosition
                    (Angle.add (Angle.decimalDegrees 90) sb)
                    (Length.metres startRadius)
                )

        trackLine = 
            GreatCircle.minorArc lastPosition currPosition
                
        startLineSide line p = 
            GreatCircle.side 
                p (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)

        isOnSameSideOfLine line p1 p2 =
            GreatCircle.side p1 (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line) == 
            GreatCircle.side p2 (GreatCircle.minorArcStart line) (GreatCircle.minorArcEnd line)

        startLineCrossing sl tl =
            mfilter 
                (isOnSameSideOfLine sl targetPosition) 
                (GreatCircle.intersection sl tl)
            
        toProgressPoint :: HorizontalPosition S84 -> ProgressPoint
        toProgressPoint _ = 
            ProgressPoint 
                (FlightTrack.time currTp)
                (FlightTrack.lat currTp)
                (FlightTrack.lon currTp)
                (FlightTrack.altitudeGps currTp)
                (Just (targetNp, distanceToTurnpoint targetNp currTp))
                
                -- (distanceToTurnpoint startNp currTp)
    in
        startLineCrossing 
        <$> (startBearing >>= startLine)
        <*> trackLine 
        >>= fmap toProgressPoint

turnpointCrossed :: (NavPoint, Turnpoint) -> NavPoint -> (TrackPoint, TrackPoint) -> Maybe ProgressPoint
turnpointCrossed (np, Cylinder radius) targetNp (lastTp, currTp) =
    let
        (NavPoint.LengthMeters npDistance) = distanceToTurnpoint np currTp
    in
        if npDistance <= radius
        then 
            Just $ ProgressPoint
                (FlightTrack.time currTp)
                (FlightTrack.lat currTp)
                (FlightTrack.lon currTp)
                (FlightTrack.altitudeGps currTp)
                (Just (targetNp, distanceToTurnpoint targetNp currTp))
        else
            Nothing

finishCrossed :: (NavPoint, TaskFinish) -> NavPoint -> (TrackPoint, TrackPoint) -> Maybe ProgressPoint
finishCrossed (np, FinishCylinder radius) prevNp (lastTp, currTp) =
    let
        (NavPoint.LengthMeters npDistance) = distanceToTurnpoint np currTp
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

finishCrossed (np, FinishLine radius) prevNp (lastTp, currTp) =
    let 
        reversedFinishBearing =
            GreatCircle.initialBearing 
                (s84position (NavPoint.lat np) (NavPoint.lon np))
                (s84position (NavPoint.lat prevNp) (NavPoint.lon prevNp))
        finishPosition = 
            s84position (NavPoint.lat np) (NavPoint.lon np)

        lastPosition = 
            s84position (FlightTrack.lat lastTp) (FlightTrack.lon lastTp)

        currPosition =
            s84position (FlightTrack.lat currTp) (FlightTrack.lon currTp)

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
        
        toProgressPoint _ =
            ProgressPoint 
                (FlightTrack.time currTp)
                (FlightTrack.lat currTp)
                (FlightTrack.lon currTp)
                (FlightTrack.altitudeGps currTp)
                Nothing

        finishCrossed = 
            GreatCircle.intersection 
                <$> (reversedFinishBearing >>= finishLine)
                <*> trackLine
                >>= fmap toProgressPoint
    in
    Nothing

advance :: [Milestone] -> ProgressPoint -> TrackPoint -> (ProgressPoint, [Milestone])
advance mss lastProgressPoint currTp =
    let
        ms = viaNonEmpty head mss
    in
        case mss of
            (np, ms) : rest -> 
                ( ProgressPoint 
                    (FlightTrack.time currTp)
                    (FlightTrack.lat currTp)
                    (FlightTrack.lon currTp)
                    (FlightTrack.altitudeGps currTp)
                    (Just (np, distanceToTurnpoint np currTp))
                , rest
                )
                
            _ -> 
                ( ProgressPoint 
                    (FlightTrack.time currTp)
                    (FlightTrack.lat currTp)
                    (FlightTrack.lon currTp)
                    (FlightTrack.altitudeGps currTp)
                    (target lastProgressPoint)
                , mss
                )


progress' :: (FlightTask, NavPoint) -> [TrackPoint] -> Maybe (ProgressPoint, NavPoint)
progress' (ft, target) tps =
    let
        nextTp = viaNonEmpty head tps

        res :: TrackPoint -> (ProgressPoint, NavPoint)
        res tp =
            ( ProgressPoint
                (FlightTrack.time tp)
                (FlightTrack.lat tp)
                (FlightTrack.lon tp)
                (FlightTrack.altitudeGps tp)
                (Just (target, distanceToTurnpoint target tp))
                -- (distanceToTurnpoint target tp)
            , target
            )
    in
    viaNonEmpty (res . head) tps

progress :: FlightTask -> NonEmpty TrackPoint -> [ProgressPoint]
progress ft tps =
    let
        (startNp, _) = FlightTask.start ft

        progressStart =
            ProgressPoint
                (FlightTrack.time $ head tps)
                (FlightTrack.lat $ head tps)
                (FlightTrack.lon $ head tps)
                (FlightTrack.altitudeGps $ head tps)
                (Just (startNp, distanceToTurnpoint startNp $ head tps))
                -- (distanceToTurnpoint startNp $ head tps)

        iter :: ProgressPoint -> TrackPoint -> ProgressPoint
        iter lastPP tp =
            ProgressPoint
                (FlightTrack.time tp)
                (FlightTrack.lat tp)
                (FlightTrack.lon tp)
                (FlightTrack.altitudeGps tp)
                (Just (startNp, distanceToTurnpoint startNp tp))
                -- (distanceToTurnpoint startNp tp)
    in
    scanl' iter progressStart (tail tps)