module TaskProgressSpec where

import Control.Arrow (ArrowChoice (..))
import Data.Geo.Jord.Geodetic qualified as Geodetic
import Data.Geo.Jord.GreatCircle qualified as GreatCircle
import Data.Geo.Jord.Length qualified as Length
import Entity (Entity (..))
import FlightTask
import FlightTrack
import FlightTrack.Parser
import Geo (Elevation (..), Latitude (..), Longitude (..), metersElevation)
import Geo.Utils (perpendicular)
import NavPoint
import ProgressPoint (ProgressPoint (altitude, target))
import Relude
import TaskProgress qualified (TaskProgress (points))
import TaskProgressUtils (progress, progressAdvance, progressInit, progressPoints, startLineCrossed)
import Test.Hspec (Spec, context, it, shouldBe)
import Text.Parsec (parse)
import TrackPoint (FixValidity (..), TrackPoint (..))
import Utils (within)

startNavPoint :: NavPoint
startNavPoint =
  NavPoint
    { name = "Start",
      code = "START",
      country = Nothing,
      lat = LatitudeDegrees 0,
      lon = LongitudeDegrees 0,
      elev = ElevationMeters 0,
      style = Unknown,
      rwdir = Nothing,
      rwlen = Nothing,
      freq = Nothing,
      desc = ""
    }

finishNavPoint :: NavPoint
finishNavPoint =
  NavPoint
    { name = "Finish",
      code = "FINISH",
      country = Nothing,
      lat = LatitudeDegrees 0,
      lon = LongitudeDegrees 0.01,
      elev = ElevationMeters 0,
      style = Unknown,
      rwdir = Nothing,
      rwlen = Nothing,
      freq = Nothing,
      desc = ""
    }

flightTask :: FlightTask
flightTask =
  FlightTask
    { start = (startNavPoint, StartLine 3000),
      turnpoints = [],
      finish = (finishNavPoint, FinishLine 3000)
    }

spec :: Spec
spec = do
  context "startLineCrossed" $ do
    it "perpendicular" $ do
      let start = Geodetic.s84Pos 0 0
          finish = Geodetic.s84Pos 0 1
          -- tp1 = Geodetic.s84Pos 0 (-0.0001)
          -- tp2 = Geodetic.s84Pos 0 0.0001

          startLine = do
            bearing <- GreatCircle.initialBearing start finish
            perpendicular start bearing (Length.metres 3000)

          lineLength =
            (GreatCircle.distance . GreatCircle.minorArcStart <$> startLine)
              <*> (GreatCircle.minorArcEnd <$> startLine)
      -- Geodetic.decimalLatitude . GreatCircle.minorArcStart
      --     <$> startLine `shouldBe` Just 0
      Geodetic.decimalLongitude
        . GreatCircle.minorArcStart
        <$> startLine
        `shouldBe` Just 0
      Geodetic.decimalLongitude
        . GreatCircle.minorArcEnd
        <$> startLine
        `shouldBe` Just 0
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
            join (GreatCircle.intersection <$> line1 <*> line2)

      Geodetic.decimalLatitude <$> intersection `shouldBe` Just 0
      Geodetic.decimalLongitude <$> intersection `shouldBe` Just 0

    it "recognises a start line crossing" $ do
      let trackPoint1 =
            TrackPoint
              { time = 0,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.0001),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }
          trackPoint2 =
            TrackPoint
              { time = 3,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees 0.0001,
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }

          result = startLineCrossed flightTask trackPoint1 trackPoint2

      isJust result `shouldBe` True

    it "does not recognise a start line crossing in the opposite direction" $ do
      let trackPoint1 =
            TrackPoint
              { time = 0,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees 0.0001,
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }
          trackPoint2 =
            TrackPoint
              { time = 3,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.0001),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }

          result = startLineCrossed flightTask trackPoint1 trackPoint2

      isNothing result `shouldBe` True

  context "progressInit" $ do
    it "initializes task progress correctly" $ do
      let trackPoint1 =
            TrackPoint
              { time = 0,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.0002),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }
          pinit = progressInit flightTask trackPoint1

      (fmap name . target . head) pinit.progressPoints `shouldBe` Just "Start"

  context "progressAdvance" $ do
    it "produces correct track point before start" $ do
      let trackPoint1 =
            TrackPoint
              { time = 0,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.001),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }
          trackPoint2 =
            TrackPoint
              { time = 3,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.0001),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }

          pinit =
            progressInit flightTask trackPoint1

          result =
            progressAdvance flightTask pinit trackPoint2

      (fmap name . target . head) result.progressPoints `shouldBe` Just "Start"

    it "produces correct track point after start" $ do
      let trackPoint1 =
            TrackPoint
              { time = 0,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees (-0.0001),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }
          trackPoint2 =
            TrackPoint
              { time = 3,
                lat = LatitudeDegrees 0,
                lon = LongitudeDegrees 0.0001,
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 1000,
                altitudeGps = ElevationMeters 1000
              }

          pinit =
            progressInit flightTask trackPoint1

          result =
            progressAdvance flightTask pinit trackPoint2

      (fmap name . target . head) result.progressPoints `shouldBe` Just "Finish"

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
            \B0722245201562N03940404EA0008700168000048" ::
              Text
          -- Either Error FlightInfo
          result = parse flightInfoParserAll "" input
          flightTrack = left show result >>= buildFlightTrack ""
          taskProgress = progress (Entity 0 flightTask) <$> flightTrack

          elevationPoints :: Either String [Double]
          elevationPoints = fmap (metersElevation . altitude) . TaskProgress.points <$> taskProgress

      length <$> result `shouldBe` Right 10
      compId <$> flightTrack `shouldBe` Right "VB"
      length . points <$> flightTrack `shouldBe` Right 8
      length . TaskProgress.points <$> taskProgress `shouldBe` Right 8

      elevationPoints `shouldBe` Right [161, 162, 163, 164, 165, 166, 167, 168]
