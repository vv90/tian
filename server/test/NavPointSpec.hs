
module NavPointSpec
    where
    
import Relude
import Test.Hspec
import NavPoint
import Text.Parsec (parse)
import Test.Hspec.QuickCheck (prop)
import Text.Printf (printf)
import Data.Geo.Jord.Angle (Angle, decimalDegrees)
import Data.Geo.Jord.Length (Length, metres)
import Data.Geo.Jord.Geodetic (Position, latLongHeightPos, HasCoordinates (decimalLatitude, decimalLongitude), height)
import Data.Geo.Jord.Models (WGS84 (WGS84))

within :: (Num a, Show a, Ord a) => a -> a -> a -> Bool
within eps x y = abs (x - y) <= eps

spec :: Spec
spec = do
    context "navPointParser" $ do
        it "parses waypoint example" $ do
            let input = "\"Cross Hands\",\"CSS\",UK,5147.809N,00405.003W,525m,1,,,,\"Turn Point, A48/A476,Between Cross Hands and Gorslas, 9 NMl ESE of Camarthen.\""
            let result = parse navPointParser "" input
            
            name <$> result `shouldBe` Right "Cross Hands"
            code <$> result `shouldBe` Right "CSS"
            country <$> result `shouldBe` Right (Just "UK")
            within 1e-5 51.7968168 . decimalLatitude . position <$> result `shouldBe` Right True
            within 1e-5 (-4.0833835) . decimalLongitude . position <$> result `shouldBe` Right True
            height . position <$> result `shouldBe` Right (metres 525)
            style <$> result `shouldBe` Right Waypoint
            rwdir <$> result `shouldBe` Right Nothing
            rwlen <$> result `shouldBe` Right Nothing
            freq <$> result `shouldBe` Right Nothing
            desc <$> result `shouldBe` Right "Turn Point, A48/A476,Between Cross Hands and Gorslas, 9 NMl ESE of Camarthen."

        it "parses waypoint example without quotes around code" $ do
            let input = "\"Cross Hands\",CSS,UK,5147.809N,00405.003W,525m,1,,,,\"Turn Point, A48/A476,Between Cross Hands and Gorslas, 9 NMl ESE of Camarthen.\""
            let result = parse navPointParser "" input
            
            code <$> result `shouldBe` Right "CSS" 

        it "parses airport example" $ do
            let input = "\"Lesce\",\"LJBL\",SI,4621.379N,01410.467E,504.0m,5,144,1130.0m,123.500,\"Home Airfield\""
            let result = parse navPointParser "" input

            name <$> result `shouldBe` Right "Lesce"
            code <$> result `shouldBe` Right "LJBL"
            country <$> result `shouldBe` Right (Just "SI")
            within 1e-5 46.356317 . decimalLatitude . position <$> result `shouldBe` Right True
            within 1e-5 14.17445 . decimalLongitude . position <$> result `shouldBe` Right True
            height . position <$> result `shouldBe` Right (metres 504)
            style <$> result `shouldBe` Right AirfieldSolid
            rwdir <$> result `shouldBe` Right (Just $ decimalDegrees 144)
            rwlen <$> result `shouldBe` Right (Just $ metres 1130)
            freq <$> result `shouldBe` Right (Just "123.500")
            desc <$> result `shouldBe` Right "Home Airfield"


    context "navPointNameParser" $ do
        it "allows commas" $ do
            parse navPointNameParser "" "\"A,B\"" `shouldBe` Right "A,B"

    context "navPointCodeParser" $ do
        it "works with quotes" $ do
            parse navPointCodeParser "" "\"A\"" `shouldBe` Right "A"

        it "works without quotes" $ do
            parse navPointCodeParser "" "A" `shouldBe` Right "A"

    context "navPointLatParser" $ do
        it "nothern hemisphere" $ do
            parse navPointLatParser "" "5107.830N" `shouldBe` Right 51.1305
        it "southern hemisphere" $ do
            parse navPointLatParser "" "5107.830S" `shouldBe` Right (-51.1305)

    context "navPointLonParser" $ do
        it "eastern hemisphere" $ do
            parse navPointLonParser "" "01410.467E" `shouldBe` Right 14.17445
        it "western hemisphere" $ do
            parse navPointLonParser "" "01410.467W" `shouldBe` Right (-14.17445)
    
    context "navPointElevationParser" $ do
        it "works for meters" $ do
            parse navPointElevationParser "" "123.456m" `shouldBe` Right 123.456
        it "fails for invalid input" $ do
            isLeft (parse navPointElevationParser "" "-.456m") `shouldBe` True
        it "converts feet to meters" $ do
            (\x -> 0.00001 < abs (x - 37.61232)) <$> parse navPointElevationParser "" "100ft" `shouldBe` Right True

    context "navPointRwdirParser" $ do
        it "parses 3 digits" $ do
            parse navPointRwdirParser "" "123" `shouldBe` Right 123

        it "parses 2 digits" $ do
            parse navPointRwdirParser "" "012" `shouldBe` Right 12

        it "parses 1 digit" $ do
            parse navPointRwdirParser "" "001" `shouldBe` Right 1

        it "fails for invalid directions" $ do
            isLeft (parse navPointRwdirParser "" "432") `shouldBe` True

    context "navPointRwlenParser" $ do
        it "parses in meters" $ do
            parse navPointRwlenParser "" "123.456m" `shouldBe` Right 123.456

        it "parses in nautical miles" $ do
            parse navPointRwlenParser "" "1.23nm" `shouldBe` Right (1.23 * 1852)

        it "parses in statute miles" $ do
            parse navPointRwlenParser "" "1.23ml" `shouldBe` Right (1.23 * 1609.344)

        it "fails for invalid units" $ do
            isLeft (parse navPointRwlenParser "" "1.23x") `shouldBe` True
        
    context "navPointStyleParser" $ do
        it "parses style correctly" $ do
            parse navPointStyleParser "" "0" `shouldBe` Right Unknown
            parse navPointStyleParser "" "1" `shouldBe` Right Waypoint
            parse navPointStyleParser "" "2" `shouldBe` Right AirfieldGrass
            parse navPointStyleParser "" "3" `shouldBe` Right Outlanding
            parse navPointStyleParser "" "4" `shouldBe` Right AirfieldGliding
            parse navPointStyleParser "" "5" `shouldBe` Right AirfieldSolid
            parse navPointStyleParser "" "6" `shouldBe` Right MountainPass
            parse navPointStyleParser "" "7" `shouldBe` Right MountainTop
            parse navPointStyleParser "" "8" `shouldBe` Right TransmitterMast
            parse navPointStyleParser "" "9" `shouldBe` Right VOR
            parse navPointStyleParser "" "10" `shouldBe` Right NDB
            parse navPointStyleParser "" "11" `shouldBe` Right CoolingTower
            parse navPointStyleParser "" "12" `shouldBe` Right Dam
            parse navPointStyleParser "" "13" `shouldBe` Right Tunnel
            parse navPointStyleParser "" "14" `shouldBe` Right Bridge
            parse navPointStyleParser "" "15" `shouldBe` Right PowerPlant
            parse navPointStyleParser "" "16" `shouldBe` Right Castle
            parse navPointStyleParser "" "17" `shouldBe` Right Intersection
        
        it "fails for invalid styles" $ do
            isLeft (parse navPointStyleParser "" "18") `shouldBe` True

    context "doubleParser" $ do
        prop "works correctly" $
            \x -> parse doubleParser "" (printf "%f" x) == Right x
