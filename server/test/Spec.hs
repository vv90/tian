module Main (main) where

-- import Lib (app)

import AprsMessageSpec qualified
import DeviceDatabaseSpec qualified
import FlightTrackSpec qualified
import LocationDatapointSpec qualified
import NavPointSpec qualified
import Relude
import TaskProgressSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- describe "Default" spec
  describe "NavPoint" NavPointSpec.spec
  describe "FlightTrack" FlightTrackSpec.spec
  describe "TaskProgress" TaskProgressSpec.spec
  describe "AprsMessage" AprsMessageSpec.spec
  describe "DeviceDatabase" DeviceDatabaseSpec.spec
  describe "LocationDatapoint" LocationDatapointSpec.spec

-- spec :: Spec
-- spec = do
--     it "runs correctly" $ do
--         10 `shouldBe` (10 :: Int)
