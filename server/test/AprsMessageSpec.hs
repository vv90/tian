module AprsMessageSpec where

import Relude
import Test.Hspec
import Aprs.AprsMessage
import Text.Parsec (parse)

spec :: Spec
spec = do
    context "AprsMessage parser" $ do
        it "parses correctly" $ do
            let input = "FLRD0095F>OGFLR,qAS,LFNE:/160721h4337.14N/00507.75E'161/061/A=001204 !W82! id06D0095F -157fpm +0.1rot 10.2dB -0.5kHz gps1x2"
                result = parse aprsMessageParser "" input

            source <$> result `shouldBe` Right "FLRD0095F"
            