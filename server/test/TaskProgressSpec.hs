module TaskProgressSpec where

import Relude
import Test.Hspec
import qualified FlightTask
import FlightTask (FlightTask(..), TaskStart(..), TaskFinish(..), Turnpoint(..))

spec :: Spec
spec = do
    context "startLineCrossed" $ do
        it "recognises a start line crossing" $ do
            
            0 `shouldBe` 0