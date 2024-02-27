module LocationDatapointSpec where

import Aprs.LocationDatapoint
import Data.Aeson (decode, encode)
import Geo
  ( AngularSpeed (..),
    Direction (DirectionDegrees),
    Elevation (..),
    Latitude (LatitudeDegrees),
    Longitude (LongitudeDegrees),
    Speed (..),
  )
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Aeson instances for LocationDatapoint" $ do
    it "should round-trip encode and decode" $ do
      let original = LocationDatapoint 123 (LatitudeDegrees 45.0) (LongitudeDegrees 90.0) (DirectionDegrees 180) (SpeedMetersPerSecond 30.0) (ElevationMeters 1000.0) (Just $ SpeedMetersPerSecond 2.0) (Just $ DegreesPerSecond 10.0)
      decode (encode original) `shouldBe` Just original

    it "should encode in the correct format" $ do
      let original = LocationDatapoint 123 (LatitudeDegrees 45.0) (LongitudeDegrees 90.0) (DirectionDegrees 180) (SpeedMetersPerSecond 30.0) (ElevationMeters 1000.0) (Just $ SpeedMetersPerSecond 2.0) (Just $ DegreesPerSecond 10.0)

      encode original `shouldBe` "[123,45,90,180,30,1000,2,10]"

    it "should encode optional values correctly" $ do
      let original = LocationDatapoint 123 (LatitudeDegrees 45.0) (LongitudeDegrees 90.0) (DirectionDegrees 180) (SpeedMetersPerSecond 30.0) (ElevationMeters 1000.0) Nothing Nothing

      encode original `shouldBe` "[123,45,90,180,30,1000,null,null]"
