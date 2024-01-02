module MapUtils exposing
    ( LineStyle(..)
    , MapItem(..)
    , MapView
    , PointStyle(..)
    , addTileSources
    , changeZoom
    , fromMercatorWeb
    , geoPointToViewCoords
    , metersPerPixel
    , normalizeTileKey
    , scaleCoords
    , scaleFromZoom
    , scaleOffset
    , stringFromTuple
    , tilesInView
    , toMercatorWeb
    , viewCoordsToGeoPoint
    )

-- import Geo.GeoUtils exposing (..)

import Api.Types exposing (..)
import Dict exposing (Dict)
import Domain.GeoUtils exposing (degreesLatitude, degreesLongitude)
import List.Extra as ListX
import Point2d exposing (Point2d)
import Tile exposing (TileKey, tileKeyToUrl, tileSize)



-- import Nav.Units exposing (Meters(..))


stringFromBool : Bool -> String
stringFromBool b =
    if b then
        "True"

    else
        "False"


stringFromTuple : ( String, String ) -> String
stringFromTuple ( fst, snd ) =
    "(" ++ fst ++ ", " ++ snd ++ ")"


type MarkerType
    = Glider


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
toMercatorWeb { lat, lon } =
    let
        latRad : Float
        latRad =
            -- lat |> (\(LatDeg latVal) -> degToRad latVal) |> getRad
            lat |> degreesLatitude |> degrees

        lonDeg : Float
        lonDeg =
            -- lon |> (\(LonDeg lonVal) -> getDeg lonVal)
            lon |> degreesLongitude

        sec : Float -> Float
        sec x =
            1 / cos x

        resY : Float
        resY =
            logBase e (tan latRad + sec latRad)
    in
    ( (lonDeg + 180) / 360, (1 - resY / pi) / 2 )


fromMercatorWeb : ( Float, Float ) -> GeoPoint
fromMercatorWeb ( x, y ) =
    let
        sinh : Float -> Float
        sinh a =
            (e ^ a - e ^ -a) / 2

        lonDeg : Float
        lonDeg =
            x * 360 - 180

        latRad : Float
        latRad =
            atan (sinh (pi * (1 - 2 * y)))

        latDeg : Float
        latDeg =
            latRad * 180 / pi
    in
    { lat = LatitudeDegrees latDeg, lon = LongitudeDegrees lonDeg }


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

        center : ( Float, Float )
        center =
            ( offsetX + toFloat mapView.width / 2, offsetY + toFloat mapView.height / 2 )
    in
    { mapView
        | zoom = mapView.zoom + logBase 2 scaleCoeffcitient
        , offset = scaleOffset center scaleCoeffcitient mapView.offset
    }


isPointInView : MapView -> ( Float, Float ) -> Bool
isPointInView mapView point =
    let
        viewRect : ( ( Float, Float ), Float, Float )
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
        scaleCoefficient : Float
        scaleCoefficient =
            scaleFromZoom mapView.zoom

        left : Float
        left =
            x * tileSize |> toFloat

        right : Float
        right =
            (x + 1) * tileSize |> toFloat

        top : Float
        top =
            y * tileSize |> toFloat

        bottom : Float
        bottom =
            (y + 1) * tileSize |> toFloat

        viewRect : ( ( Float, Float ), Float, Float )
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
        fTileSize : Float
        fTileSize =
            toFloat tileSize

        zoom : Int
        zoom =
            floor mapView.zoom

        scaleCoefficient : Float
        scaleCoefficient =
            scaleFromZoom mapView.zoom

        ( offsetX, offsetY ) =
            scaleCoords scaleCoefficient mapView.offset

        width : Float
        width =
            toFloat mapView.width / scaleCoefficient

        height : Float
        height =
            toFloat mapView.height / scaleCoefficient

        minTileX : Int
        minTileX =
            offsetX / fTileSize |> floor

        minTileY : Int
        minTileY =
            offsetY / fTileSize |> floor

        maxTileX : Int
        maxTileX =
            (offsetX + width) / fTileSize |> floor

        maxTileY : Int
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
        zoom : Int
        zoom =
            floor mapView.zoom

        ( offsetX, offsetY ) =
            mapView.offset

        scaleCoefficient : Float
        scaleCoefficient =
            scaleFromZoom mapView.zoom

        toCoord : Float -> Float
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
        zoom : Int
        zoom =
            floor mapView.zoom

        ( offsetX, offsetY ) =
            mapView.offset

        scaleCoefficient : Float
        scaleCoefficient =
            scaleFromZoom mapView.zoom

        fromCoord : Float -> Float
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
        n : Int
        n =
            2 ^ zoom
    in
    x >= 0 && x < n && y >= 0 && y < n


normalizeTileKey : TileKey -> TileKey
normalizeTileKey ( x, y, zoom ) =
    let
        n : Int
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
        applyScale : Float -> Float
        applyScale x =
            x / scaleCoeffcitient
    in
    Tuple.mapBoth applyScale applyScale offset



-- earthCircumference * cos (degrees lat) / tileLength = 2 ^ zoom
-- log2 (earthCircumference * cos (degrees lat) / tileLength) = zoom


metersPerPixel : Int -> Maybe Distance
metersPerPixel zoom =
    let
        mpp : Maybe Float
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
