module ExampleTest exposing (testFromMercator)

import Api.Types exposing (..)
import Domain.GeoUtils exposing (degreesLatitude)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as ListX
import MapUtils exposing (fromMercatorWeb)
import Test exposing (Test, describe, test)


testFromMercator : Test
testFromMercator =
    describe "context"
        [ test "latMin" <|
            \_ ->
                let
                    latMin : Float
                    latMin =
                        fromMercatorWeb ( 1250 / (2 ^ 11), 674 / (2 ^ 11) ) |> .lat |> degreesLatitude
                in
                Expect.within (Absolute 1.0e-13) latMin 52.268157373768176
        , test "latMax" <|
            \_ ->
                let
                    latMax : Float
                    latMax =
                        fromMercatorWeb ( 1251 / (2 ^ 11), 675 / (2 ^ 11) ) |> .lat |> degreesLatitude
                in
                Expect.within (Absolute 1.0e-13) latMax 52.160454557747045
        ]
