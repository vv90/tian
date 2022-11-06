module MapUtils exposing (..)

-- import Geo.GeoUtils exposing (..)

import Api.Geo exposing (Distance(..), Latitude(..), Longitude(..))
import Dict exposing (Dict)
import List.Extra as ListX



-- import Nav.Units exposing (Meters(..))


tileSize : Int
tileSize =
    256


minZoom : Float
minZoom =
    0


maxZoom : Float
maxZoom =
    19


stringFromBool : Bool -> String
stringFromBool b =
    if b then
        "True"

    else
        "False"


stringFromTuple : ( String, String ) -> String
stringFromTuple ( fst, snd ) =
    "(" ++ fst ++ ", " ++ snd ++ ")"


type alias TileKey =
    ( Int, Int, Int )


type MarkerType
    = Glider


type alias GeoPoint =
    ( Latitude, Longitude )


type PointStyle
    = TaskPoint
    | TrackPoint


type LineStyle
    = TaskLine
    | TrackLine


type MapItem
    = Point PointStyle GeoPoint
    | Circle GeoPoint Distance
    | Line LineStyle (List GeoPoint)
    | Marker GeoPoint String



-- | Polygon (List GeoPoint)
-- type alias Marker =
--     { position : GeoPoint
--     , markerType : MarkerType
--     , caption : String
--     }


type alias MapView =
    { width : Int
    , height : Int
    , zoom : Float
    , offset : ( Float, Float )
    }


toMercatorWeb : GeoPoint -> ( Float, Float )
toMercatorWeb ( LatitudeDegrees lat, LongitudeDegrees lon ) =
    let
        latRad =
            -- lat |> (\(LatDeg latVal) -> degToRad latVal) |> getRad
            degrees lat

        lonDeg =
            -- lon |> (\(LonDeg lonVal) -> getDeg lonVal)
            lon

        sec x =
            1 / cos x

        resY =
            logBase e (tan latRad + sec latRad)
    in
    ( (lonDeg + 180) / 360, (1 - resY / pi) / 2 )


fromMercatorWeb : ( Float, Float ) -> GeoPoint
fromMercatorWeb ( x, y ) =
    let
        sinh a =
            (e ^ a - e ^ -a) / 2

        lonDeg =
            x * 360 - 180

        latRad =
            atan (sinh (pi * (1 - 2 * y)))

        latDeg =
            latRad * 180 / pi
    in
    ( LatitudeDegrees latDeg, LongitudeDegrees lonDeg )


scaleOffset : ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float )
scaleOffset ( x, y ) scale ( offsetX, offsetY ) =
    ( (scale - 1) * x + offsetX
    , (scale - 1) * y + offsetY
    )



-- |> Tuple.mapBoth round round


changeZoom : Float -> MapView -> MapView
changeZoom scaleCoeffcitient mapView =
    let
        ( offsetX, offsetY ) =
            mapView.offset

        center =
            ( offsetX + toFloat mapView.width / 2, offsetY + toFloat mapView.height / 2 )
    in
    { mapView
        | zoom = mapView.zoom + logBase 2 scaleCoeffcitient
        , offset = scaleOffset center scaleCoeffcitient mapView.offset
    }


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


isPointInView : MapView -> ( Float, Float ) -> Bool
isPointInView mapView point =
    let
        viewRect =
            ( mapView.offset, toFloat mapView.width, toFloat mapView.height )
    in
    isPointInRect viewRect point


isPointInRect : ( ( number, number ), number, number ) -> ( number, number ) -> Bool
isPointInRect ( ( rectX, rectY ), width, height ) ( px, py ) =
    px
        >= rectX
        && px
        <= rectX
        + width
        && py
        >= rectY
        && py
        <= rectY
        + height


isInView : MapView -> TileKey -> Bool
isInView mapView ( x, y, zoom ) =
    let
        scaleCoefficient =
            scaleFromZoom mapView.zoom

        left =
            x * tileSize |> toFloat

        right =
            (x + 1) * tileSize |> toFloat

        top =
            y * tileSize |> toFloat

        bottom =
            (y + 1) * tileSize |> toFloat

        viewRect =
            ( scaleCoords scaleCoefficient mapView.offset
            , toFloat mapView.width / scaleCoefficient
            , toFloat mapView.height / scaleCoefficient
            )
    in
    toFloat zoom
        <= mapView.zoom
        && toFloat zoom
        > mapView.zoom
        - 1
        && (isPointInRect viewRect ( left, top )
                || isPointInRect viewRect ( left, bottom )
                || isPointInRect viewRect ( right, top )
                || isPointInRect viewRect ( right, bottom )
           )


tilesInView : MapView -> List TileKey
tilesInView mapView =
    let
        fTileSize =
            toFloat tileSize

        zoom =
            floor mapView.zoom

        scaleCoefficient =
            scaleFromZoom mapView.zoom

        ( offsetX, offsetY ) =
            scaleCoords scaleCoefficient mapView.offset

        width =
            toFloat mapView.width / scaleCoefficient

        height =
            toFloat mapView.height / scaleCoefficient

        minTileX =
            offsetX / fTileSize |> floor

        minTileY =
            offsetY / fTileSize |> floor

        maxTileX =
            (offsetX + width) / fTileSize |> floor

        maxTileY =
            (offsetY + height) / fTileSize |> floor
    in
    ListX.lift2
        (\x y -> ( x, y, zoom ))
        (List.range minTileX maxTileX)
        (List.range minTileY maxTileY)


geoPointToViewCoords : MapView -> GeoPoint -> ( Float, Float )
geoPointToViewCoords mapView point =
    let
        zoom =
            floor mapView.zoom

        ( offsetX, offsetY ) =
            mapView.offset

        scaleCoefficient =
            scaleFromZoom mapView.zoom

        toCoord n =
            n * toFloat (2 ^ zoom) * toFloat tileSize * scaleCoefficient

        -- (x, y) = toMercatorWeb point |> Tuple.mapBoth (toCoord >> round) (toCoord >> round)
        -- toCoord x = x * toFloat (tileSize * 2^zoom) / scaleCoefficient
    in
    -- (x, y)
    toMercatorWeb point
        |> Tuple.mapBoth
            (toCoord >> (\x -> x - offsetX))
            (toCoord >> (\y -> y - offsetY))


viewCoordsToGeoPoint : MapView -> ( Float, Float ) -> GeoPoint
viewCoordsToGeoPoint mapView coords =
    let
        zoom =
            floor mapView.zoom

        ( offsetX, offsetY ) =
            mapView.offset

        scaleCoefficient =
            scaleFromZoom mapView.zoom

        fromCoord n =
            n / (toFloat tileSize * toFloat (2 ^ zoom) * scaleCoefficient)
    in
    coords
        |> Tuple.mapBoth
            ((\x -> x + offsetX) >> fromCoord)
            ((\y -> y + offsetY) >> fromCoord)
        |> fromMercatorWeb


isValidTileKey : TileKey -> Bool
isValidTileKey ( x, y, zoom ) =
    let
        n =
            2 ^ zoom
    in
    x >= 0 && x < n && y >= 0 && y < n


normalizeTileKey : TileKey -> TileKey
normalizeTileKey ( x, y, zoom ) =
    let
        n =
            2 ^ zoom
    in
    ( modBy n x, modBy n y, zoom )


addTileSources : MapView -> Dict TileKey String -> Dict TileKey String
addTileSources mapView tilesDict =
    tilesInView mapView
        |> List.filter isValidTileKey
        |> List.map (\tileKey -> ( tileKey, tileKeyToUrl tileKey ))
        |> Dict.fromList
        |> Dict.union tilesDict


scaleFromZoom : Float -> Float
scaleFromZoom zoom =
    2 ^ (zoom - (floor >> toFloat) zoom)


scaleCoords : Float -> ( Float, Float ) -> ( Float, Float )
scaleCoords scaleCoeffcitient offset =
    let
        applyScale x =
            x / scaleCoeffcitient
    in
    Tuple.mapBoth applyScale applyScale offset


metersPerPixel : Int -> Maybe Distance
metersPerPixel zoom =
    let
        mpp =
            case zoom of
                0 ->
                    Just 156412

                1 ->
                    Just 78206

                2 ->
                    Just 39103

                3 ->
                    Just 19551

                4 ->
                    Just 9776

                5 ->
                    Just 4888

                6 ->
                    Just 2444

                7 ->
                    Just 1222

                8 ->
                    Just 610.984

                9 ->
                    Just 305.492

                10 ->
                    Just 152.746

                11 ->
                    Just 76.373

                12 ->
                    Just 38.187

                13 ->
                    Just 19.093

                14 ->
                    Just 9.547

                15 ->
                    Just 4.773

                16 ->
                    Just 2.387

                17 ->
                    Just 1.193

                18 ->
                    Just 0.596

                19 ->
                    Just 0.298

                20 ->
                    Just 0.149

                _ ->
                    Nothing
    in
    Maybe.map DistanceMeters mpp


earthRadius : Distance
earthRadius =
    DistanceMeters 6372798.2
