module TaskProgressSpec where

import Relude
import Test.Hspec ( context, it, shouldBe, Spec )
import qualified FlightTask
import qualified TaskProgress
import FlightTask
import NavPoint 
import FlightTrack
import Geo
import Geo.Utils (perpendicular)
import TaskProgress (startLineCrossed)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Length as Length
import Utils (within)

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
            
                result = startLineCrossed flightTask (trackPoint1, trackPoint2)
            
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
            
                result = startLineCrossed flightTask (trackPoint1, trackPoint2)

            isNothing result `shouldBe` True
