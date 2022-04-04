module Example exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Geo.GeoUtils exposing (GeoPoint, Latitude(..), Longitude(..), toSpherical, fromSpherical, getLon, getLat)
import Nav.Units exposing (Deg(..), getDeg)
import Math.Vector3 as V3

angle : (Float, Float, Float) -> (Float, Float, Float) -> Float
angle (x1, y1, z1) (x2, y2, z2) =
  let
    l1 = sqrt (x1^2 + y1^2 + z1^2)
    l2 = sqrt (x2^2 + y2^2 + z2^2)
    dotProduct = x1 * x2 + y1 * y2 + z1 * z2
  in
    acos (dotProduct / (l1 * l2))

testAngle : Test
testAngle = 
  test "angle between x and y axis is 90deg" <|
    \_ -> 
      let
        xaxis = (1, 0, 0)
        yaxis = (0, 1, 0)
      in
        Expect.within (Absolute 0.00000001) (90 * pi / 180) (angle xaxis yaxis)

testToSpherical : Test
testToSpherical =
  describe "toSpherical"
    [ test "spherical coords conversion round trip" <|
        \_ -> 
          let
            geoPoint = 
              GeoPoint 
                ((Deg >> LonDeg) 39.94354248046875) 
                ((Deg >> LatDeg) 52.16550896167103)
            
            spherical = toSpherical geoPoint
            res = fromSpherical spherical
        in
          Expect.all 
            [ \({lon}) -> 
                Expect.within 
                  (Absolute 0.00000001) 
                  ((getLon >> getDeg) lon)
                  ((getLon >> getDeg) geoPoint.lon)
            , \({lat}) ->
                Expect.within
                  (Absolute 0.00000001)
                  ((getLat >> getDeg) lat)
                  ((getLat >> getDeg) geoPoint.lat)
            ]
            res

    , test "toSpherical returns vector with length 1" <|
        \_ -> 
          let
            geoPoint = 
              GeoPoint 
                ((Deg >> LonDeg) 39.94354248046875) 
                ((Deg >> LatDeg) 52.16550896167103)
            
            v = toSpherical geoPoint
          in
            Expect.within (Absolute 0.000001) (V3.length v) 1
    ]
  
