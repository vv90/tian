module FlightTrackSpec where

import Data.Time
import FlightTrack.Parser
import Geo
import Relude
import Test.Hspec
import Text.Parsec (parse)
import TrackPoint (FixValidity (..), TrackPoint (..))

truncateLat :: Latitude -> Latitude
truncateLat (LatitudeDegrees d) = LatitudeDegrees $ roundN (6::Int) d

truncateLon :: Longitude -> Longitude
truncateLon (LongitudeDegrees d) = LongitudeDegrees $ roundN (6::Int) d

spec :: Spec
spec = do
  context "trackDateParser" $ do
    it "parses date correctly" $ do
      let input = "HFDTE010211" :: Text
      let result = parse (trackDateIdentifier *> trackDateParser) "" input
      let expected = FlightDate $ UTCTime (fromGregorian 2011 2 1) 0
      result `shouldBe` Right expected

    it "parses alternative date format" $ do
      let input = "HFDTEDATE:050521,01" :: Text
      let result = parse (trackDateIdentifier *> trackDateParser) "" input
      let expected = FlightDate $ UTCTime (fromGregorian 2021 5 5) 0
      result `shouldBe` Right expected

  context "compIdParser" $ do
    it "parses competition id correctly" $ do
      let input = "HFCIDCOMPETITIONID:AB3\n" :: Text
      let result = parse (compIdIdentifier *> compIdParser) "" input

      result `shouldBe` Right (CompId "AB3")

  context "trackPointTimeParser" $ do
    it "parses track point time" $ do
      let input = "160240" :: Text
      let result = parse trackPointTimeParser "" input

      result `shouldBe` Right (secondsToDiffTime 57760)

  context "trackPointParser" $ do
    it "parses track point" $ do
      let input = "B1602405407121N00249342WA002800042120509950" :: Text
      let result = 
            (\res -> res { lat = truncateLat res.lat , lon = truncateLon res.lon }) <$>
            parse (trackPointIdentifier *> trackPointParser) "" input
      let expected =
            TrackPoint
              { time = 57760 ,
                lat = LatitudeDegrees 54.118683,
                lon = LongitudeDegrees (-2.822367),
                fixValidity = Gps3D,
                altitudeBaro = ElevationMeters 280,
                altitudeGps = ElevationMeters 421
              }

      result `shouldBe` Right expected
