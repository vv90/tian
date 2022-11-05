module Example exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testToSpherical : Test
testToSpherical =
    describe "context"
        [ test "functionality" <|
            \_ ->
                Expect.equal 0 0
        ]
