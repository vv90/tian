module TaskProgressSpec where

import Relude
import Test.Hspec ( context, it, shouldBe, Spec )
import qualified FlightTask
import qualified TaskProgress (TaskProgress(points))
import FlightTask
import NavPoint 
import FlightTrack
import Geo
import Geo.Utils (perpendicular)
import TaskProgressUtils ( progressInit, startLineCrossed, progressAdvance, progress, progressPoints )
import ProgressPoint ( ProgressPoint(target, altitude) )
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Length as Length
import Utils (within)
import Text.Parsec (parse)
import Control.Arrow (ArrowChoice(..))
import Entity (Entity(..))

startNavPoint = 
    NavPoint
        { name = "Start"
        , code = "START"
        , country = Nothing
        , lat = LatitudeDegrees 0
        , lon = LongitudeDegrees 0 
        , elev = ElevationMeters 0
        , style = Unknown
        , rwdir = Nothing 
        , rwlen = Nothing
        , freq = Nothing
        , desc = ""
        }
finishNavPoint = 
    NavPoint
        { name = "Finish"
        , code = "FINISH"
        , country = Nothing
        , lat = LatitudeDegrees 0
        , lon = LongitudeDegrees 0.01
        , elev = ElevationMeters 0
        , style = Unknown
        , rwdir = Nothing 
        , rwlen = Nothing
        , freq = Nothing
        , desc = ""
        }
flightTask = 
    FlightTask 
        { start = (startNavPoint, StartLine 3000)
        , turnpoints = []
        , finish = (finishNavPoint, FinishLine 3000)
        }

spec :: Spec
spec = do
    context "startLineCrossed" $ do
        it "perpendicular" $ do
            let start = Geodetic.s84Pos 0 0
                finish = Geodetic.s84Pos 0 1
                tp1 = Geodetic.s84Pos 0 (-0.0001)
                tp2 = Geodetic.s84Pos 0 0.0001
            
                startLine = do 
                    bearing <- GreatCircle.initialBearing start finish
                    perpendicular start bearing (Length.metres 3000)

                lineLength =
                    GreatCircle.distance 
                    <$> (GreatCircle.minorArcStart <$> startLine)
                    <*> (GreatCircle.minorArcEnd <$> startLine)
            -- Geodetic.decimalLatitude . GreatCircle.minorArcStart 
            --     <$> startLine `shouldBe` Just 0
            Geodetic.decimalLongitude . GreatCircle.minorArcStart
                <$> startLine `shouldBe` Just 0
            Geodetic.decimalLongitude . GreatCircle.minorArcEnd 
                <$> startLine `shouldBe` Just 0
            within 1e-4 6000 . Length.toMetres <$> lineLength `shouldBe` Just True

        it "intersection" $ do
            let line1 = 
                    GreatCircle.minorArc 
                        (Geodetic.s84Pos 0.0001 0)
                        (Geodetic.s84Pos (-0.0001) 0)
               
                line2 = 
                    GreatCircle.minorArc 
                        (Geodetic.s84Pos 0 (-0.0001))
                        (Geodetic.s84Pos 0 0.0001)
                
                intersection =
                    GreatCircle.intersection <$> line1 <*> line2 >>= id

            Geodetic.decimalLatitude <$> intersection `shouldBe` Just 0
            Geodetic.decimalLongitude <$> intersection `shouldBe` Just 0
                

        it "recognises a start line crossing" $ do
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
                trackPoint2 =
                    TrackPoint
                        { time = 3 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees 0.0001
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
            
                result = startLineCrossed flightTask trackPoint1 trackPoint2
            
            isJust result `shouldBe` True

        it "does not recognise a start line crossing in the opposite direction" $ do
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
                trackPoint2 =
                    TrackPoint
                        { time = 3 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        } 
            
                result = startLineCrossed flightTask trackPoint1 trackPoint2

            isNothing result `shouldBe` True

    context "progressInit" $ do
        it "initializes task progress correctly" $ do
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0002)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
                pinit = progressInit flightTask trackPoint1

            (fmap (name . fst) . target . head . progressPoints) pinit `shouldBe` Just "Start"
            (fmap (metersDistance . snd) . target . last . progressPoints) pinit `shouldBe` Just 22.239016
            0 `shouldBe` 0

    context "progressAdvance" $ do
        it "produces correct track point before start" $ do
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
                trackPoint2 =
                    TrackPoint
                        { time = 3 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }

                pinit = 
                    progressInit flightTask trackPoint1

                result =
                    progressAdvance flightTask pinit trackPoint2

            (fmap (name . fst) . target . head . progressPoints) result `shouldBe` Just "Start"
            (fmap (metersDistance . snd) . target . head . progressPoints) result `shouldBe` Just 11.119508

        it "produces correct track point after start" $ do
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
                trackPoint2 =
                    TrackPoint
                        { time = 3 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        } 

                pinit = 
                    progressInit flightTask trackPoint1

                result =
                    progressAdvance flightTask pinit trackPoint2

            (fmap (name . fst) . target . head . progressPoints) result `shouldBe` Just "Finish"
            (fmap (metersDistance . snd) . target . head . progressPoints) result `shouldBe` Just 1100.831289

    context "progress" $ do
        it "produces correct progress results" $ do
            let input = 
                    "HFDTEDATE:050521,01\n\
                    \HFCIDCOMPETITIONID:VB\n\
                    \B0721565201562N03940404EA0008700161000024\n\
                    \B0722025201562N03940404EA0008700162000054\n\
                    \B0722045201562N03940403EA0008700163100106\n\
                    \B0722085201562N03940404EV0008700164500164\n\
                    \B0722125201562N03940404EV0008700165500118\n\
                    \B0722165201562N03940404EA0008700166066060\n\
                    \B0722205201562N03940404EA0008700167000046\n\
                    \B0722245201562N03940404EA0008700168000048"
                result = parse flightInfoParser "" input
                flightTrack = left show result >>= buildFlightTrack 
                taskProgress = progress (Entity 0 flightTask) <$> flightTrack

                elevationPoints :: Either String [Double]
                elevationPoints = fmap (metersElevation . altitude) . TaskProgress.points <$> taskProgress

            length <$> result `shouldBe` Right 10
            compId <$> flightTrack `shouldBe` Right "VB"
            length . points <$> flightTrack `shouldBe` Right 8
            length . TaskProgress.points <$> taskProgress `shouldBe` Right 8
            
            elevationPoints `shouldBe` Right [161, 162, 163, 164, 165, 166, 167, 168] 
