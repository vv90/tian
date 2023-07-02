module FlightTrackSpec where

import Control.Arrow (ArrowChoice (..))
import Data.Time
import Entity (Entity (..))
import FlightTrack
import FlightTrack.Parser
import Geo
import NavPoint
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Parsec (parse)
import Text.Printf (printf)
import TrackPoint (FixValidity (..), TrackPoint (..))

spec :: Spec
spec = do
  context "trackDateParser" $ do
    it "parses date correctly" $ do
      let input = "HFDTE010211"
      let result = parse (trackDateIdentifier *> trackDateParser) "" input
      let expected = FlightDate $ UTCTime (fromGregorian 2011 2 1) 0
      result `shouldBe` Right expected

    it "parses alternative date format" $ do
      let input = "HFDTEDATE:050521,01"
      let result = parse (trackDateIdentifier *> trackDateParser) "" input
      let expected = FlightDate $ UTCTime (fromGregorian 2021 5 5) 0
      result `shouldBe` Right expected

  context "compIdParser" $ do
    it "parses competition id correctly" $ do
      let input = "HFCIDCOMPETITIONID:AB3\n"
      let result = parse (compIdIdentifier *> compIdParser) "" input

      result `shouldBe` Right (CompId "AB3")

  context "trackPointTimeParser" $ do
    it "parses track point time" $ do
      let input = "160240"
      let result = parse trackPointTimeParser "" input

      result `shouldBe` Right (secondsToDiffTime 57760)

  context "trackPointParser" $ do
    it "parses track point" $ do
      let input = "B1602405407121N00249342WA002800042120509950"
      let result = parse (trackPointIdentifier *> trackPointParser) "" input
      let expected =
            TrackPoint
              57760
              (LatitudeDegrees 54.118683)
              (LongitudeDegrees (-2.822367))
              Gps3D
              (ElevationMeters 280)
              (ElevationMeters 421)

      result `shouldBe` Right expected
