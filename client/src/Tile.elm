module Tile exposing (..)

import Api.Types exposing (..)
import Constants exposing (earthCircumference)


tileSize : Int
tileSize =
    256


minZoom : Float
minZoom =
    0


maxZoom : Float
maxZoom =
    19


type ZoomLevel
    = Z0
    | Z1
    | Z2
    | Z3
    | Z4
    | Z5
    | Z6
    | Z7
    | Z8
    | Z9
    | Z10
    | Z11
    | Z12
    | Z13
    | Z14
    | Z15
    | Z16
    | Z17
    | Z18
    | Z19
    | Z20


type alias TileKey =
    ( Int, Int, Int )


type alias Tile =
    { x : Int
    , y : Int
    , zoom : ZoomLevel
    }


fromTileKey : TileKey -> Tile
fromTileKey ( x, y, zoom ) =
    { x = x
    , y = y
    , zoom = zoomLevel zoom
    }


toTileKey : Tile -> TileKey
toTileKey { x, y, zoom } =
    ( x, y, zoomInt zoom )


tileKeyToUrl : TileKey -> String
tileKeyToUrl ( x, y, zoom ) =
    String.concat
        [ "http://a.tile.openstreetmap.org/"
        , String.fromInt zoom
        , "/"
        , x |> String.fromInt
        , "/"
        , y |> String.fromInt
        , ".png"
        ]


tileLength : Latitude -> ZoomLevel -> Float
tileLength (LatitudeDegrees lat) zoom =
    earthCircumference * cos (degrees lat) / 2 ^ toFloat (zoomInt zoom)


zoomInt : ZoomLevel -> Int
zoomInt zoom =
    case zoom of
        Z0 ->
            0

        Z1 ->
            1

        Z2 ->
            2

        Z3 ->
            3

        Z4 ->
            4

        Z5 ->
            5

        Z6 ->
            6

        Z7 ->
            7

        Z8 ->
            8

        Z9 ->
            9

        Z10 ->
            10

        Z11 ->
            11

        Z12 ->
            12

        Z13 ->
            13

        Z14 ->
            14

        Z15 ->
            15

        Z16 ->
            16

        Z17 ->
            17

        Z18 ->
            18

        Z19 ->
            19

        Z20 ->
            20


zoomLevel : Int -> ZoomLevel
zoomLevel zoom =
    case zoom of
        0 ->
            Z0

        1 ->
            Z1

        2 ->
            Z2

        3 ->
            Z3

        4 ->
            Z4

        5 ->
            Z5

        6 ->
            Z6

        7 ->
            Z7

        8 ->
            Z8

        9 ->
            Z9

        10 ->
            Z10

        11 ->
            Z11

        12 ->
            Z12

        13 ->
            Z13

        14 ->
            Z14

        15 ->
            Z15

        16 ->
            Z16

        17 ->
            Z17

        18 ->
            Z18

        19 ->
            Z19

        20 ->
            Z20

        z ->
            if z < 0 then
                Z0

            else
                Z20
