module Mercator exposing (..)

import Api.Geo exposing (Latitude)
import Math.Matrix4 exposing (Mat4)
import Tile exposing (ZoomLevel(..), tileLength)


type Mercator
    = Mercator


mercatorToSim : Latitude -> ( Float, Float ) -> ( Float, Float )
mercatorToSim lat ( x, y ) =
    let
        toMeters n =
            tileLength lat Z0 * n
    in
    ( toMeters x, negate (toMeters y) )


simToMercator : Latitude -> ( Float, Float ) -> ( Float, Float )
simToMercator lat ( x, y ) =
    let
        fromMeters n =
            n / tileLength lat Z0
    in
    ( fromMeters x, negate (fromMeters y) )
