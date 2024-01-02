module Map3dUtils exposing
    ( Map3dItem(..)
    , MercatorCoords(..)
    , MercatorUnit(..)
    , PlaneCoords(..)
    , WorldCoords(..)
    , fromMercatorPoint
    , getMercatorUnit
    , makeMesh_
    , makeTilePoints
    , makeTiles
    , mercatorFrame
    , mercatorRate
    , tileMesh
    , tileRectangle
    , toMercatorPoint
    )

import Api.Types exposing (..)
import Array exposing (Array)
import Array.Extra
import Color exposing (Color)
import Common.ApiCommands exposing (hydrateTile)
import Dict exposing (Dict)
import Direction3d
import Domain.GeoUtils exposing (metersDistance)
import Frame2d exposing (Frame2d)
import Length exposing (Length, Meters)
import List.Extra as ListX
import MapUtils exposing (fromMercatorWeb, metersPerPixel, toMercatorWeb)
import Maybe.Extra
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Rate)
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d exposing (SketchPlane3d)
import Tile exposing (Tile, TileKey, ZoomLevel(..), tileLength, zoomInt)
import TriangularMesh exposing (TriangularMesh)
import Vector2d exposing (Vector2d)



-- import Scene3d.Mesh as Mesh


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


type MercatorUnit
    = MercatorUnit


mercatorUnit : Float -> Quantity Float MercatorUnit
mercatorUnit =
    Quantity


getMercatorUnit : Quantity Float MercatorUnit -> Float
getMercatorUnit (Quantity n) =
    n


type MercatorCoords
    = MercatorCoords


type WorldCoords
    = WorldCoords


type PlaneCoords
    = PlaneCoords


type SomeCoords
    = SomeCoords



-- mercatorToMeters : Latitude -> Quantity Float MercatorUnit -> Quantity Float Meters
-- mercatorToMeters lat (Quantity n) =
--     tileLength lat Z0 * n |> Length.meters
-- mercatorToSimCoords : Latitude -> Point2d MercatorUnit MercatorCoords -> Point2d Meters WorldCoords
-- mercatorToSimCoords lat p =
--     let
--         ( x, y ) =
--             ( Point2d.xCoordinate p, Point2d.yCoordinate p )
--         f = Frame2d.atOrigin |> Frame2d.reverseY
--     in
--     Point2d.
-- toWorldPoint : GeoPoint -> Point2d Meters WorldCoords
-- toWorldPoint gp =
--     let
--         lat =
--             Tuple.first gp
--         p : Point2d Meters WorldCoords
--         p =
--             toMercatorWeb gp
--                 |> Point2d.fromTuple (mercatorUnit >> mercatorToMeters lat)
--         -- |> Tuple.mapBoth (mercatorUnit >> mercatorToMeters lat) (mercatorUnit >> mercatorToMeters lat)
--     in
--     p


toMercatorPoint : GeoPoint -> Point2d MercatorUnit MercatorCoords
toMercatorPoint =
    toMercatorWeb >> Point2d.fromTuple mercatorUnit


fromMercatorPoint : Point2d MercatorUnit MercatorCoords -> GeoPoint
fromMercatorPoint =
    Point2d.toTuple getMercatorUnit >> fromMercatorWeb


tileOrigin : Tile -> Point2d MercatorUnit MercatorCoords
tileOrigin tile =
    let
        numTiles : Int
        numTiles =
            2 ^ zoomInt tile.zoom

        ( x, y ) =
            ( toFloat tile.x / toFloat numTiles
            , toFloat tile.y / toFloat numTiles
            )
    in
    Point2d.xy (mercatorUnit x) (mercatorUnit y)


tileRectangle : Tile -> ( Point2d MercatorUnit MercatorCoords, Point2d MercatorUnit MercatorCoords )
tileRectangle tile =
    let
        numTiles : Int
        numTiles =
            2 ^ zoomInt tile.zoom

        ( x0, y0 ) =
            ( toFloat tile.x / toFloat numTiles
            , toFloat tile.y / toFloat numTiles
            )

        ( x1, y1 ) =
            ( toFloat (tile.x + 1) / toFloat numTiles
            , toFloat (tile.y + 1) / toFloat numTiles
            )
    in
    ( Point2d.xy (mercatorUnit x0) (mercatorUnit y0)
    , Point2d.xy (mercatorUnit x1) (mercatorUnit y1)
    )



-- containingTile : ZoomLevel -> Point2d MercatorUnit MercatorCoords -> TileKey
-- containingTile zoom p =
--     let
--         numTiles =
--             2 ^ zoomInt zoom
--         ( Quantity x, Quantity y ) =
--             ( Point2d.xCoordinate p, Point2d.yCoordinate p )
--     in
--     ( floor (x * toFloat numTiles)
--     , floor (y * toFloat numTiles)
--     , zoomInt zoom
--     )


mercatorRate : Latitude -> Quantity Float (Rate Meters MercatorUnit)
mercatorRate lat =
    -- length (0 to earchCircumference) in meters adjusted for the latitude
    -- corresponds to mercator unit (0 to 1)
    Quantity.rate (Length.meters (tileLength lat Z0)) (mercatorUnit 1)


mercatorFrame : GeoPoint -> Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
mercatorFrame center =
    let
        rate : Quantity Float (Rate Meters MercatorUnit)
        rate =
            mercatorRate center.lat

        ( cx, cy ) =
            toMercatorWeb center

        -- translating the frame so that the `center` point coordinates become (0, 0) in the rendered world coordinates
        transVector : Vector2d MercatorUnit PlaneCoords
        transVector =
            Vector2d.xy (mercatorUnit cx) (mercatorUnit (negate cy))
                |> Vector2d.reverse

        flatWorldFrame : Frame2d Meters PlaneCoords {}
        flatWorldFrame =
            Frame2d.atOrigin
    in
    -- in mercator coordinates the Y axis is reversed
    Frame2d.at_ rate flatWorldFrame
        |> Frame2d.translateBy transVector
        |> Frame2d.reverseY



-- Frame2d.at_ rate flatWorldFrame
-- |> Frame2d.reverseY
-- |> Frame2d.translateBy transVector


makeTiles :
    (Point2d Meters PlaneCoords -> Bool)
    -> Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    -> Quantity Float (Rate Meters MercatorUnit)
    -> Point2d Meters PlaneCoords
    -> ZoomLevel
    -> List ( TileKey, ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) )
makeTiles isInView mFrame mRate focalPoint zoom =
    let
        numTiles : Int
        numTiles =
            2 ^ zoomInt zoom

        -- mFrame =
        --     mercatorFrame center
        -- mRate =
        --     mercatorRate (Tuple.first center)
        containingTile : Point2d MercatorUnit MercatorCoords -> TileKey
        containingTile p =
            ( floor (getMercatorUnit (Point2d.xCoordinate p) * toFloat numTiles)
            , floor (getMercatorUnit (Point2d.yCoordinate p) * toFloat numTiles)
            , zoomInt zoom
            )

        tileCoords : TileKey -> ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords )
        tileCoords ( kx, ky, _ ) =
            ( Point2d.xyIn
                mFrame
                (mercatorUnit (toFloat kx / toFloat numTiles))
                (mercatorUnit (toFloat ky / toFloat numTiles))
                |> Point2d.at mRate
            , Point2d.xyIn
                mFrame
                (mercatorUnit (toFloat (kx + 1) / toFloat numTiles))
                (mercatorUnit (toFloat (ky + 1) / toFloat numTiles))
                |> Point2d.at mRate
            )

        firstTile : TileKey
        firstTile =
            focalPoint
                |> Point2d.at_ mRate
                |> Point2d.relativeTo mFrame
                |> containingTile

        isCoordsInView : ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) -> Bool
        isCoordsInView ( p0, p1 ) =
            isInView p0
                || isInView p1
                || isInView (Point2d.xy (Point2d.xCoordinate p0) (Point2d.yCoordinate p1))
                || isInView (Point2d.xy (Point2d.xCoordinate p1) (Point2d.yCoordinate p0))

        expandTiles : Dict TileKey ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) -> TileKey -> Dict TileKey ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords )
        expandTiles dict ( x, y, z ) =
            [ ( x - 1, y + 1, z )
            , ( x, y + 1, z )
            , ( x + 1, y + 1, z )
            , ( x - 1, y, z )
            , ( x + 1, y, z )
            , ( x - 1, y - 1, z )
            , ( x, y - 1, z )
            , ( x + 1, y - 1, z )
            ]
                |> List.filter
                    (\( x1, y1, _ ) -> x1 >= 0 && y1 >= 0 && x1 < numTiles && y1 < numTiles)
                |> List.foldl
                    (\key d ->
                        let
                            ( p0, p1 ) =
                                tileCoords key
                        in
                        if not (Dict.member key d) && isCoordsInView ( p0, p1 ) then
                            expandTiles (Dict.insert key ( p0, p1 ) d) key

                        else
                            d
                    )
                    dict
    in
    expandTiles
        (Dict.singleton firstTile (tileCoords firstTile))
        firstTile
        |> Dict.toList


makeTilePoints : Tile -> List GeoPoint
makeTilePoints tile =
    let
        numTiles : Int
        numTiles =
            2 ^ zoomInt tile.zoom

        ( xCount, yCount ) =
            ( 9, 9 )

        ( xStart, yStart ) =
            ( toFloat tile.x / toFloat numTiles
            , toFloat tile.y / toFloat numTiles
            )

        ( xEnd, yEnd ) =
            ( toFloat (tile.x + 1) / toFloat numTiles
            , toFloat (tile.y + 1) / toFloat numTiles
            )

        ( xStep, yStep ) =
            ( (xEnd - xStart) / xCount
            , (yEnd - yStart) / yCount
            )

        makePoint : Int -> Int -> Point2d MercatorUnit coordinates
        makePoint i j =
            Point2d.xy
                (mercatorUnit <| xStart + toFloat i * xStep)
                -- the data points and texture orientation is top left to bottom right
                -- while the grid indexing is bottom left to top right
                -- so invert the y axis to match the orientation
                (mercatorUnit <| yStart + toFloat j * yStep)

        -- |> Point2d.at mRate
    in
    List.range 0 yCount
        |> ListX.andThen (\j -> List.range 0 xCount |> List.map (\i -> makePoint i j))
        |> List.map fromMercatorPoint


makeMesh_ :
    Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    -> Quantity Float (Rate Meters MercatorUnit)
    -> SketchPlane3d Meters WorldCoords { defines : PlaneCoords }
    -> ElevationPointsTile
    -> Mesh.Textured WorldCoords
makeMesh_ mFrame mRate xyPlane tile =
    let
        points : Array ( GeoPoint, Int )
        points =
            hydrateTile tile

        numRows : Int
        numRows =
            Array.length tile.elevations // tile.rowLength

        yCount : Int
        yCount =
            numRows - 1

        xCount : Int
        xCount =
            tile.rowLength - 1

        in3d : Quantity Float Meters -> Point2d Meters PlaneCoords -> Point3d Meters WorldCoords
        in3d elev p =
            Point3d.on xyPlane p
                |> Point3d.translateIn Direction3d.positiveZ elev

        makePoint : ( GeoPoint, Int ) -> Point3d Meters WorldCoords
        makePoint ( p, e ) =
            p
                |> toMercatorPoint
                |> Point2d.placeIn mFrame
                |> Point2d.at mRate
                |> in3d (Length.meters <| toFloat e)

        meshOriginPoint : () -> Point3d Meters WorldCoords
        meshOriginPoint () =
            points
                |> Array.get 0
                |> Maybe.map makePoint
                |> Maybe.withDefault Point3d.origin

        withRelativeTextureCoords :
            ( Float, Float )
            -> Point3d Meters WorldCoords
            -> { position : Point3d Meters WorldCoords, uv : ( Float, Float ) }
        withRelativeTextureCoords ( dx, dy ) p =
            { position = p
            , uv = ( dx, -dy )
            }

        pointAt : Int -> Int -> { position : Point3d Meters WorldCoords, uv : ( Float, Float ) }
        pointAt i j =
            points
                |> Array.get (j * tile.rowLength + i)
                -- |> Maybe.andThen (Array.get i)
                |> Maybe.Extra.unpack meshOriginPoint makePoint
                |> withRelativeTextureCoords ( toFloat i / toFloat xCount, toFloat j / toFloat yCount )
    in
    TriangularMesh.indexedGrid xCount yCount pointAt
        |> Mesh.texturedFacets


tileMesh :
    SketchPlane3d Meters WorldCoords { defines : PlaneCoords }
    -> Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    -> Quantity Float (Rate Meters MercatorUnit)
    -> List Float
    -> Tile
    -> Mesh.Textured WorldCoords
tileMesh xyPlane mFrame mRate elevValues tile =
    let
        numTiles : Int
        numTiles =
            2 ^ zoomInt tile.zoom

        ( xCount, yCount ) =
            ( 9, 9 )

        ( xStart, yStart ) =
            ( toFloat tile.x / toFloat numTiles
            , toFloat tile.y / toFloat numTiles
            )

        ( xEnd, yEnd ) =
            ( toFloat (tile.x + 1) / toFloat numTiles
            , toFloat (tile.y + 1) / toFloat numTiles
            )

        ( xStep, yStep ) =
            ( (xEnd - xStart) / xCount
            , (yEnd - yStart) / yCount
            )

        elevAt : Int -> Int -> Maybe Length
        elevAt i j =
            elevValues
                |> ListX.getAt (j * xCount + i)
                |> Maybe.map Length.meters

        -- |> Point3d.on xyPlane
        -- |> Point3d.translateIn Direction3d.positiveZ elev
        -- 8 9   10 11
        -- 4  5  6  7
        -- 0  1  2  3
        in3d : Maybe (Quantity Float Meters) -> Point2d Meters PlaneCoords -> Point3d Meters WorldCoords
        in3d elev p =
            case elev of
                Just z ->
                    Point3d.on xyPlane p
                        |> Point3d.translateIn Direction3d.positiveZ z

                Nothing ->
                    Point3d.on xyPlane p

        withRelativeTextureCoords : ( a, number ) -> b -> { position : b, uv : ( a, number ) }
        withRelativeTextureCoords ( dx, dy ) p =
            { position = p
            , uv = ( dx, -dy )
            }

        -- pointAt1 i j =
        --     Point2d.xyIn
        --         mFrame
        --         (mercatorUnit <| xStart + toFloat i * xStep)
        --         (mercatorUnit <| yStart + toFloat j * yStep)
        --         |> Point2d.at mRate
        pointAt : Int -> Int -> { position : Point3d Meters WorldCoords, uv : ( Float, Float ) }
        pointAt i j =
            Point2d.xyIn
                mFrame
                (mercatorUnit <| xStart + toFloat i * xStep)
                -- the data points and texture orientation is top left to bottom right
                -- while the grid indexing is bottom left to top right
                -- so invert the y axis to match the orientation
                (mercatorUnit <| yStart + toFloat j * yStep)
                |> Point2d.at mRate
                |> in3d (elevAt i j)
                |> withRelativeTextureCoords ( toFloat i / xCount, toFloat j / yCount )
    in
    TriangularMesh.indexedGrid xCount yCount pointAt
        |> Mesh.texturedFacets



-- simToMercatorCoords : Latitude -> ( Float, Float ) -> ( Float, Float )
-- simToMercatorCoords lat ( x, y ) =
--     let
--         fromMeters n =
--             n / tileLength lat Z0
--     in
--     ( fromMeters x, negate (fromMeters y) )
-- localToGeoPoint : GeoPoint -> ( Float, Float ) -> GeoPoint
-- localToGeoPoint origin ( x, y ) =
--     let
--         lat =
--             Tuple.first origin
--         tlen =
--             tileLength lat Z0
--         -- ( cx, cy ) =
--         --     toMercatorWeb origin
--         ( px, py ) =
--             mercatorToMeters
--                 lat
--                 (toMercatorWeb origin)
--     in
--     fromMercatorWeb
--         ( (px + x) / tlen
--         , (py - y) / tlen
--         )
-- tileCoords : GeoPoint -> ( Int, Int ) -> ZoomLevel -> ( ( Float, Float ), ( Float, Float ) )
-- tileCoords origin ( x, y ) zoom =
--     let
--         numTiles =
--             2 ^ zoomInt zoom
--         ( cx, cy ) =
--             toMercatorWeb origin
--         -- mercator coords origin is top left
--         -- but local coords origin is bottom left
--         -- so we need to invert y axis
--         ( px, py ) =
--             mercatorToMeters
--                 (Tuple.first origin)
--                 ( toFloat x / toFloat numTiles - cx
--                 , cy - toFloat y / toFloat numTiles
--                 )
--         tLen =
--             tileLength (Tuple.first origin) zoom
--     in
--     -- mercator tile origin point is also top left corner
--     -- need to change it to bottom left
--     ( ( px, py - tLen ), ( px + tLen, py ) )
-- fillTiles : (( Float, Float ) -> Bool) -> GeoPoint -> ZoomLevel -> List ( TileKey, ( ( Float, Float ), ( Float, Float ) ) )
-- fillTiles isInView center zoom =
--     let
--         ( cx, cy ) =
--             toMercatorWeb center
--         numTiles =
--             2 ^ zoomInt zoom
--         -- tLen =
--         --     tileLength (Tuple.first center) zoom
--         ( tx, ty ) =
--             ( cx * toFloat numTiles
--             , cy * toFloat numTiles
--             )
--         -- ( offsetX, offsetY ) =
--         --     ( tx * tLen
--         --     , ty * tLen
--         --     )
--         firstTileKey =
--             ( floor tx
--             , floor ty
--             )
--         firstTileCoords =
--             tileCoords center firstTileKey zoom
--         -- centerTileKey =
--         --     ( floor tx
--         --     , floor ty
--         --     , zoomInt zoom
--         --     )
--         -- centerTileCoords =
--         --     let
--         --         ( x0, y0 ) =
--         --             ( (toFloat (floor tx) - tx) * tLen
--         --             , (toFloat (floor ty) - ty) * tLen
--         --             )
--         --     in
--         --     ( ( x0, y0 )
--         --     , ( x0 + tLen, y0 + tLen )
--         --     )
--         isCoordsInView : ( ( Float, Float ), ( Float, Float ) ) -> Bool
--         isCoordsInView ( ( x0, y0 ), ( x1, y1 ) ) =
--             isInView ( x0, y0 )
--                 || isInView ( x1, y0 )
--                 || isInView ( x0, y1 )
--                 || isInView ( x1, y1 )
--         nearbyTiles : Dict ( Int, Int ) ( ( Float, Float ), ( Float, Float ) ) -> ( ( Int, Int ), ( ( Float, Float ), ( Float, Float ) ) ) -> Dict ( Int, Int ) ( ( Float, Float ), ( Float, Float ) )
--         nearbyTiles dict ( ( x, y ), ( ( x0, y0 ), ( x1, y1 ) ) ) =
--             -- [ ( ( x - 1, y + 1, z ), ( ( x0 - tLen, y0 - tLen ), ( x0, y0 ) ) )
--             -- , ( ( x, y + 1, z ), ( ( x0, y0 - tLen ), ( x1, y0 ) ) )
--             -- , ( ( x + 1, y + 1, z ), ( ( x1, y0 - tLen ), ( x1 + tLen, y0 ) ) )
--             -- , ( ( x - 1, y, z ), ( ( x0 - tLen, y0 ), ( x0, y1 ) ) )
--             -- , ( ( x + 1, y, z ), ( ( x1, y0 ), ( x1 + tLen, y1 ) ) )
--             -- , ( ( x - 1, y - 1, z ), ( ( x0 - tLen, y1 ), ( x0, y1 + tLen ) ) )
--             -- , ( ( x, y - 1, z ), ( ( x0, y1 ), ( x1, y1 + tLen ) ) )
--             -- , ( ( x + 1, y - 1, z ), ( ( x1, y1 ), ( x1 + tLen, y1 + tLen ) ) )
--             -- ]
--             [ ( x - 1, y + 1 )
--             , ( x, y + 1 )
--             , ( x + 1, y + 1 )
--             , ( x - 1, y )
--             , ( x + 1, y )
--             , ( x - 1, y - 1 )
--             , ( x, y - 1 )
--             , ( x + 1, y - 1 )
--             ]
--                 |> List.filter
--                     (\( a, b ) ->
--                         a
--                             >= 0
--                             && b
--                             >= 0
--                             && a
--                             < numTiles
--                             && b
--                             < numTiles
--                      -- && isTileInView ( a, b )
--                     )
--                 |> List.foldl
--                     (\key d ->
--                         let
--                             ( p0, p1 ) =
--                                 tileCoords center key zoom
--                         in
--                         if not (Dict.member key d) && isCoordsInView ( p0, p1 ) then
--                             nearbyTiles (Dict.insert key ( p0, p1 ) d) ( key, ( p0, p1 ) )
--                         else
--                             d
--                     )
--                     dict
--         -- tileOffset =
--         --     ( toFloat (floor tx) - tx
--         --     , toFloat (floor ty) - ty
--         --     )
--         -- pos =
--         --     ( centerTileKey
--         --     , (toFloat (floor tx) - tx) * tl
--         --     , (toFloat (floor ty) - ty) * tl
--         --     )
--         -- expand (tk, tx, ty) =
--         --     0
--     in
--     -- Maybe.withDefault [] (Maybe.map (\p -> [ p ]) pos)
--     nearbyTiles
--         (Dict.singleton firstTileKey firstTileCoords)
--         ( firstTileKey, firstTileCoords )
--         |> Dict.toList
--         |> List.map (\( ( x, y ), coords ) -> ( ( x, y, zoomInt zoom ), coords ))
