module Map3dUtils exposing (..)

import Api.Geo exposing (Distance(..), Elevation, Latitude(..), Longitude(..))
import Common.GeoUtils exposing (GeoPoint, metersDistance)
import Dict exposing (Dict)
import MapUtils exposing (TileKey, ZoomLevel(..), earthCircumference, metersPerPixel, tileLength, tileSize, toMercatorWeb, zoomInt)
import Svg.Attributes exposing (cx)


type Map3dItem
    = Point GeoPoint Elevation
    | Marker String GeoPoint Elevation
    | Line (List ( GeoPoint, Elevation ))
    | Cylinder GeoPoint Distance Elevation



-- | Wall GeoPoint GeoPoint Elevation
-- getTiles : ( ( Distance, Distance ), ( Distance, Distance ) ) -> ZoomLevel -> GeoPoint -> List TileKey
-- getTiles ( ( DistanceMeters fromX, DistanceMeters fromY ), ( DistanceMeters toX, DistanceMeters toY ) ) zoom center =
--     let
--         ( cx, cy ) =
--             toMercatorWeb center
--         numTiles =
--             2 ^ zoomInt zoom
--         tLen =
--             tileLength zoom
--         ( centerTileX, centerTileY ) =
--             ( cx * toFloat numTiles |> floor
--             , cy * toFloat numTiles |> floor
--               -- , zoomInt zoom
--             )
--         diff =
--             ceiling <| fromX / tLen
--         firstTileKey =
--             centerTileX
--     in
--     []


type alias ViewInfo =
    { zoom : ZoomLevel
    , center : GeoPoint
    , azimuth : Float
    , elevation : Float
    , distance : Distance
    }


mercatorToMeters : Latitude -> ( Float, Float ) -> ( Float, Float )
mercatorToMeters lat ( x, y ) =
    let
        toMeters n =
            tileLength lat Z0 * n
    in
    ( toMeters x, toMeters y )


tileCoords : GeoPoint -> ( Int, Int ) -> ZoomLevel -> ( ( Float, Float ), ( Float, Float ) )
tileCoords origin ( x, y ) zoom =
    let
        numTiles =
            2 ^ zoomInt zoom

        ( cx, cy ) =
            toMercatorWeb origin

        -- mercator coords origin is top left
        -- but local coords origin is bottom left
        -- so we need to invert y axis
        ( px, py ) =
            mercatorToMeters
                (Tuple.first origin)
                ( toFloat x / toFloat numTiles - cx
                , cy - toFloat y / toFloat numTiles
                )

        tLen =
            tileLength (Tuple.first origin) zoom
    in
    -- mercator tile origin point is also top left corner
    -- need to change it to bottom left
    ( ( px, py - tLen ), ( px + tLen, py ) )


fillTiles : (( Float, Float ) -> Bool) -> GeoPoint -> ZoomLevel -> List ( TileKey, ( ( Float, Float ), ( Float, Float ) ) )
fillTiles isInView center zoom =
    let
        ( cx, cy ) =
            toMercatorWeb center

        numTiles =
            2 ^ zoomInt zoom

        tLen =
            tileLength (Tuple.first center) zoom

        ( tx, ty ) =
            ( cx * toFloat numTiles
            , cy * toFloat numTiles
            )

        -- ( offsetX, offsetY ) =
        --     ( tx * tLen
        --     , ty * tLen
        --     )
        firstTileKey =
            ( floor tx
            , floor ty
            )

        firstTileCoords =
            tileCoords center firstTileKey zoom

        -- centerTileKey =
        --     ( floor tx
        --     , floor ty
        --     , zoomInt zoom
        --     )
        -- centerTileCoords =
        --     let
        --         ( x0, y0 ) =
        --             ( (toFloat (floor tx) - tx) * tLen
        --             , (toFloat (floor ty) - ty) * tLen
        --             )
        --     in
        --     ( ( x0, y0 )
        --     , ( x0 + tLen, y0 + tLen )
        --     )
        isCoordsInView : ( ( Float, Float ), ( Float, Float ) ) -> Bool
        isCoordsInView ( ( x0, y0 ), ( x1, y1 ) ) =
            isInView ( x0, y0 )
                || isInView ( x1, y0 )
                || isInView ( x0, y1 )
                || isInView ( x1, y1 )

        nearbyTiles : Dict ( Int, Int ) ( ( Float, Float ), ( Float, Float ) ) -> ( ( Int, Int ), ( ( Float, Float ), ( Float, Float ) ) ) -> Dict ( Int, Int ) ( ( Float, Float ), ( Float, Float ) )
        nearbyTiles dict ( ( x, y ), ( ( x0, y0 ), ( x1, y1 ) ) ) =
            -- [ ( ( x - 1, y + 1, z ), ( ( x0 - tLen, y0 - tLen ), ( x0, y0 ) ) )
            -- , ( ( x, y + 1, z ), ( ( x0, y0 - tLen ), ( x1, y0 ) ) )
            -- , ( ( x + 1, y + 1, z ), ( ( x1, y0 - tLen ), ( x1 + tLen, y0 ) ) )
            -- , ( ( x - 1, y, z ), ( ( x0 - tLen, y0 ), ( x0, y1 ) ) )
            -- , ( ( x + 1, y, z ), ( ( x1, y0 ), ( x1 + tLen, y1 ) ) )
            -- , ( ( x - 1, y - 1, z ), ( ( x0 - tLen, y1 ), ( x0, y1 + tLen ) ) )
            -- , ( ( x, y - 1, z ), ( ( x0, y1 ), ( x1, y1 + tLen ) ) )
            -- , ( ( x + 1, y - 1, z ), ( ( x1, y1 ), ( x1 + tLen, y1 + tLen ) ) )
            -- ]
            [ ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            ]
                |> List.filter
                    (\( a, b ) ->
                        a
                            >= 0
                            && b
                            >= 0
                            && a
                            < numTiles
                            && b
                            < numTiles
                     -- && isTileInView ( a, b )
                    )
                |> List.foldl
                    (\key d ->
                        let
                            ( p0, p1 ) =
                                tileCoords center key zoom
                        in
                        if not (Dict.member key d) && isCoordsInView ( p0, p1 ) then
                            nearbyTiles (Dict.insert key ( p0, p1 ) d) ( key, ( p0, p1 ) )

                        else
                            d
                    )
                    dict

        -- tileOffset =
        --     ( toFloat (floor tx) - tx
        --     , toFloat (floor ty) - ty
        --     )
        -- pos =
        --     ( centerTileKey
        --     , (toFloat (floor tx) - tx) * tl
        --     , (toFloat (floor ty) - ty) * tl
        --     )
        -- expand (tk, tx, ty) =
        --     0
    in
    -- Maybe.withDefault [] (Maybe.map (\p -> [ p ]) pos)
    nearbyTiles
        (Dict.singleton firstTileKey firstTileCoords)
        ( firstTileKey, firstTileCoords )
        |> Dict.toList
        |> List.map (\( ( x, y ), coords ) -> ( ( x, y, zoomInt zoom ), coords ))
