module Components.Map3dUtils exposing
    ( Map3dItem(..)
    , MercatorCoords(..)
    , MercatorUnit(..)
    , PlaneCoords(..)
    , WorldCoords(..)
    , fromMercatorPoint
    , makeMesh
    , makeTiles
    , mercatorFrame
    , mercatorRate
    , toMercatorPoint
    )

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiCommands exposing (hydrateTile)
import Dict exposing (Dict)
import Direction3d
import Domain.GeoUtils exposing (fromMercatorWeb, toMercatorWeb)
import Frame2d exposing (Frame2d)
import Length exposing (Meters)
import Maybe.Extra
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Rate)
import Scene3d.Mesh as Mesh
import SketchPlane3d exposing (SketchPlane3d)
import Tile exposing (TileKey, ZoomLevel(..), tileLength, zoomInt)
import TriangularMesh
import Vector2d exposing (Vector2d)


type Map3dItem
    = Point GeoPoint Elevation
    | Marker String GeoPoint Elevation
    | Line (List ( GeoPoint, Elevation ))
    | Cylinder GeoPoint Distance Elevation


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


toMercatorPoint : GeoPoint -> Point2d MercatorUnit MercatorCoords
toMercatorPoint =
    toMercatorWeb >> Point2d.fromTuple mercatorUnit


fromMercatorPoint : Point2d MercatorUnit MercatorCoords -> GeoPoint
fromMercatorPoint =
    Point2d.toTuple getMercatorUnit >> fromMercatorWeb


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


makeMesh :
    Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    -> Quantity Float (Rate Meters MercatorUnit)
    -> SketchPlane3d Meters WorldCoords { defines : PlaneCoords }
    -> ElevationPointsTile
    -> Mesh.Textured WorldCoords
makeMesh mFrame mRate xyPlane tile =
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
                |> Maybe.Extra.unpack meshOriginPoint makePoint
                |> withRelativeTextureCoords ( toFloat i / toFloat xCount, toFloat j / toFloat yCount )
    in
    TriangularMesh.indexedGrid xCount yCount pointAt
        |> Mesh.texturedFacets
