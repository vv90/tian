module TaskProgressSpec where

import Relude
import Test.Hspec ( context, it, shouldBe, Spec )
import qualified FlightTask
import qualified TaskProgress
import FlightTask
import NavPoint 
import FlightTrack
import Geo
import TaskProgress (startLineCrossed)

spec :: Spec
spec = do
    context "startLineCrossed" $ do
        it "recognises a start line crossing" $ do
            let startNavPoint = 
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
            let finishNavPoint = 
                    NavPoint
                        { name = "Finish"
                        , code = "FINISH"
                        , country = Nothing
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees 1
                        , elev = ElevationMeters 0
                        , style = Unknown
                        , rwdir = Nothing 
                        , rwlen = Nothing
                        , freq = Nothing
                        , desc = ""
                        }
            let trackPoint1 =
                    TrackPoint
                        { time = 0 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees (-0.0001)
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
            let trackPoint2 =
                    TrackPoint
                        { time = 3 
                        , lat = LatitudeDegrees 0
                        , lon = LongitudeDegrees 0.0001
                        , fixValidity = Gps3D
                        , altitudeBaro = ElevationMeters 1000
                        , altitudeGps = ElevationMeters 1000
                        }
            let flightTask = 
                    FlightTask 
                        { start = (startNavPoint, StartLine 3000)
                        , turnpoints = []
                        , finish = (finishNavPoint, FinishLine 3000)
                        }
            let result = startLineCrossed flightTask (trackPoint1, trackPoint2)
            
            TaskProgress.lat <$> result `shouldBe` Just (LatitudeDegrees 0)
            TaskProgress.lon <$> result `shouldBe` Just (LongitudeDegrees 0)
