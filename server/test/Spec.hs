{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Relude
-- import Lib (app)
import Test.Hspec
import qualified NavPointSpec
import qualified FlightTrackSpec
import qualified TaskProgressSpec

main :: IO ()
main = hspec $ do
    -- describe "Default" spec
    describe "NavPoint" NavPointSpec.spec
    describe "FlightTrack" FlightTrackSpec.spec
    describe "TaskProgress" TaskProgressSpec.spec

-- spec :: Spec 
-- spec = do
--     it "runs correctly" $ do
--         10 `shouldBe` (10 :: Int)
