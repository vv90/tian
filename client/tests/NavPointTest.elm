module NavPointTest exposing (..)

import Expect
import Geo.GeoUtils exposing (GeoPoint, Latitude(..), Longitude(..))
import Nav.NavPoint exposing (Elev(..), NavPoint, WaypointStyle(..), navPointParser)
import Nav.Units exposing (Deg(..), Meters(..))
import Parser
import Test exposing (..)


testNavPointParser : Test
testNavPointParser =
    describe "navPointPraser"
        [ test "parses valid nav point correctly" <|
            \_ ->
                let
                    raw =
                        "\"GRJAZI\",GRJAZI,RU,5226.210N,03954.354E,139.0m,2,,,\"131.500\",\"GRJAZI 05/23 48x1462 131.5MGc\""

                    result =
                        Parser.run navPointParser raw

                    expected =
                        NavPoint
                            "GRJAZI"
                            "GRJAZI"
                            (Just "RU")
                            (GeoPoint (LonDeg (Deg 39.9059)) (LatDeg (Deg 52.43683333333333)))
                            (ElevMeters 139)
                            AirfieldGrass
                            Nothing
                            Nothing
                            (Just ( 131, 500 ))
                            "GRJAZI 05/23 48x1462 131.5MGc"
                in
                Expect.equal result (Ok expected)
        ]
