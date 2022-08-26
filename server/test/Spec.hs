{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Relude
-- import Lib (app)
import Test.Hspec
import qualified NavPointSpec

main :: IO ()
main = hspec $ do
    -- describe "Default" spec
    describe "NavPoint" NavPointSpec.spec

-- spec :: Spec 
-- spec = do
--     it "runs correctly" $ do
--         10 `shouldBe` (10 :: Int)
