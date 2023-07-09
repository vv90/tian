module Map3d exposing (..)

import Angle exposing (Angle)
import Api.Geo exposing (Distance(..), Elevation(..), Latitude(..), Longitude(..))
import Axis3d
import Basics.Extra exposing (uncurry)
import Browser.Events as BE
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Common.ApiCommands exposing (loadElevationsCmd)
import Common.ApiResult exposing (ApiResult, DeferredResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredToMaybe, setPending)
import Common.GeoUtils exposing (GeoPoint, metersDistance)
import Dict exposing (Dict)
import Dict.Extra as DictX
import Direction3d
import Flags exposing (WindowSize)
import Frame2d exposing (Frame2d)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as D
import Length exposing (Length, Meters)
import List.Extra as ListX
import Map3dUtils exposing (Map3dItem(..), MercatorCoords, MercatorUnit, PlaneCoords(..), ViewInfo, WorldCoords, fromMercatorPoint, getMercatorUnit, makeTiles, mercatorFrame, mercatorRate, tileMesh, toMercatorPoint)
import MapUtils exposing (TileKey, ZoomLevel(..), earthCircumference, tileKeyToUrl, tileLength, toMercatorWeb, zoomInt, zoomLevel)
import Maybe.Extra as MaybeX
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Projection
import Quantity exposing (Quantity(..), Rate, minus, plus)
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d exposing (SketchPlane3d)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task
import Viewpoint3d



-- type WorldCoordinates
--     = WorldCoordinates


type WebGLResult
    = Web


type DragState
    = MovingFrom (Point3d Meters WorldCoords)
    | Static


type alias ViewArgs =
    { focalPoint : Point3d Meters WorldCoords
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float Meters
    }


type alias TileData =
    { texture : Deferred (Material.Texture Color)
    , mesh : Deferred (Mesh.Textured WorldCoords) -- todo: add Result to support refetching in case of error
    }


type alias Model =
    { windowSize : WindowSize

    -- , origin : GeoPoint
    , mapFrame : Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    , mercatorRate : Quantity Float (Rate Meters MercatorUnit)
    , dragState : DragState
    , viewArgs : ViewArgs
    , loadedTiles : Dict TileKey TileData

    -- , loadedTiles : Dict TileKey (Maybe (Material.Texture Color))
    , displayedTiles : List ( TileKey, ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) )
    }


withPendingTextures : List TileKey -> Model -> Model
withPendingTextures keys model =
    let
        setPendingTexture item =
            case item of
                Just data ->
                    Just { data | texture = setPending data.texture }

                Nothing ->
                    Just { texture = InProgress, mesh = NotStarted }

        -- updateItem key dict =
        --     Dict.update key setPendingTexture dict
        updatedTiles =
            keys
                |> List.foldl (\key dict -> Dict.update key setPendingTexture dict) model.loadedTiles
    in
    { model | loadedTiles = updatedTiles }


withPendingMeshes : List TileKey -> Model -> Model
withPendingMeshes keys model =
    let
        setPendingMesh item =
            case item of
                Just data ->
                    Just { data | mesh = setPending data.mesh }

                Nothing ->
                    Just { texture = NotStarted, mesh = InProgress }

        updatedTiles =
            keys
                |> List.foldl (\key dict -> Dict.update key setPendingMesh dict) model.loadedTiles
    in
    { model | loadedTiles = updatedTiles }


withFocalPoint : Point3d Meters WorldCoords -> ViewArgs -> ViewArgs
withFocalPoint focalPoint viewArgs =
    { viewArgs | focalPoint = focalPoint }


adjustViewDistance : Length -> ViewArgs -> ViewArgs
adjustViewDistance delta viewArgs =
    let
        newDistance =
            viewArgs.distance |> Quantity.plus delta

        minDistance =
            Length.meters 100
    in
    { viewArgs | distance = Quantity.max newDistance minDistance }


xyPlane : SketchPlane3d Meters WorldCoords { defines : PlaneCoords }
xyPlane =
    SketchPlane3d.xy


pickZoom : Model -> ZoomLevel
pickZoom model =
    let
        ( LatitudeDegrees lat, _ ) =
            SketchPlane3d.originPoint xyPlane
                |> Point3d.projectInto xyPlane
                |> Point2d.at_ model.mercatorRate
                |> Point2d.relativeTo model.mapFrame
                |> fromMercatorPoint

        viewDistance =
            Length.inMeters model.viewArgs.distance
    in
    logBase 2 (earthCircumference * cos (degrees lat) / viewDistance)
        |> round
        |> zoomLevel



-- loadMissingTilesCmd : Dict TileKey TileData -> List TileKey -> Cmd Msg
-- loadMissingTilesCmd dict keys =
--     keys
--         |> List.filter
--             (\tileKey -> not (Dict.member tileKey dict))
--         >> List.map
--             (\tileKey ->
--                 Material.load (tileKeyToUrl tileKey)
--                     |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)
--             )
--         >> Cmd.batch


loadTextureCmd : TileKey -> Cmd Msg
loadTextureCmd tileKey =
    tileKey
        |> tileKeyToUrl
        |> Material.load
        |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)


updateTiles : Model -> ( Model, Cmd Msg )
updateTiles model =
    let
        numTiles =
            round (toFloat model.windowSize.width / 256)

        cmr =
            camera model.viewArgs

        sRect =
            screenRectangle model.windowSize

        pLeft =
            Camera3d.ray cmr sRect (Point2d.pixels 0 0)
                |> Axis3d.intersectionWithPlane Plane3d.xy

        pRight =
            Camera3d.ray cmr sRect (Point2d.pixels (toFloat model.windowSize.width) 0)
                |> Axis3d.intersectionWithPlane Plane3d.xy

        dist =
            Maybe.map2 Point3d.distanceFrom pLeft pRight

        tLen =
            Maybe.map (Length.inMeters >> (\d -> d / toFloat numTiles)) dist

        ( LatitudeDegrees lat, _ ) =
            SketchPlane3d.originPoint xyPlane
                |> Point3d.projectInto xyPlane
                |> Point2d.at_ model.mercatorRate
                |> Point2d.relativeTo model.mapFrame
                |> fromMercatorPoint

        zoom =
            Maybe.map ((\l -> logBase 2 (earthCircumference * cos (degrees lat) / l)) >> round >> zoomLevel) tLen

        -- focusedGeoPoint =
        --     Point3d.toMeters model.viewArgs.focalPoint
        --         |> (\{ x, y } -> localToGeoPoint model.origin ( x, y ))
        tiles =
            zoom
                |> MaybeX.unwrap
                    []
                    (makeTiles
                        (isInView model.viewArgs model.windowSize)
                        model.mapFrame
                        model.mercatorRate
                        (Point3d.projectInto xyPlane model.viewArgs.focalPoint)
                    )

        -- gets tile keys that are missing the given property
        missingKeys prop =
            List.filterMap
                (\( tk, _ ) ->
                    case Maybe.map prop (Dict.get tk model.loadedTiles) of
                        Just NotStarted ->
                            Just tk

                        Just _ ->
                            Nothing

                        Nothing ->
                            Just tk
                )

        missingTextures =
            missingKeys .texture tiles

        missingMeshes =
            missingKeys .mesh tiles

        newModel =
            { model | displayedTiles = tiles }
                |> withPendingTextures missingTextures
                |> withPendingMeshes missingMeshes

        cmds =
            loadElevationsCmd (Finished >> LoadElevations missingMeshes) missingMeshes
                :: (missingTextures |> List.map loadTextureCmd)

        -- missingKeys ts =
        --     List.filter
        --         (\( tileKey, _ ) -> not (Dict.member tileKey model.loadedTiles))
        --         ts
        --         |> List.map Tuple.first
        -- cmds keys =
        -- loadElevationsCmd (Finished >> LoadElevations keys) keys
        -- List.map
        --     (\tileKey ->
        --         tileKey
        --             |> tileKeyToUrl
        --             |> Material.load
        --             |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)
        --     )
        --     keys
        -- cmds keys =
        --     (missingKeys .mesh keys |> (\ks -> loadElevationsCmd (Finished >> LoadElevations ks) ks))
        --         :: (missingKeys .texture keys |> List.map loadTextureCmd)
        -- missingTextures =
        --     List.filterMap
        --         (\(tk, _) ->
        --             case Maybe.map (.texture) (Dict.get tk model.loadedTiles) of
        --                 Just (NotStarted) -> Just tk
        --                 Just _ -> Nothing
        --                 Nothing -> Just tk
        --         )
        -- missingMeshes =
        --     List.filterMap
        --         (\(tk, _) ->
        --             case Maybe.map (.mesh) (Dict.get tk model.loadedTiles) of
        --                 Just (NotStarted) -> Just tk
        --                 Just _ -> Nothing
        --                 Nothing -> Just tk
        --         )
        -- loadElevationsCmd (Finished >> LoadElevations (missingKeys ts)) (missingKeys ts)
        -- :: List.map
        --         (\tileKey ->
        --             tileKey
        --                 |> tileKeyToUrl
        --                 |> Material.load
        --                 |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)
        --         )
        --         (missingKeys ts)
        -- cmd =
        --     List.filter
        --         (\( tileKey, _ ) -> not (Dict.member tileKey model.loadedTiles))
        --         >> List.map
        --             (\( tileKey, _ ) ->
        --                 Material.load (tileKeyToUrl tileKey)
        --                     |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)
        --             )
        --         >> Cmd.batch
    in
    -- Maybe.map (\ts -> ( newModel ts, ts |> cmds |> Cmd.batch )) tiles
    ( newModel, Cmd.batch cmds )


camera : ViewArgs -> Camera3d Meters WorldCoords
camera viewArgs =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.orbitZ viewArgs
        , viewportHeight = viewArgs.distance

        -- , verticalFieldOfView = Angle.degrees 30
        }


isInView : ViewArgs -> WindowSize -> Point2d Meters PlaneCoords -> Bool
isInView viewArgs windowSize p =
    let
        p3d =
            -- Point3d.meters x y 0
            Point3d.on xyPlane p

        sRect =
            screenRectangle windowSize

        cmr =
            camera viewArgs

        projectedPoint =
            Projection.toScreenSpace
                cmr
                sRect
                p3d

        depth =
            Projection.depth cmr p3d
    in
    Rectangle2d.contains projectedPoint sRect
        && Quantity.greaterThan (Length.meters 0) depth
        && Quantity.lessThan (Length.meters 200000) depth


init : WindowSize -> GeoPoint -> ( Model, Cmd Msg )
init windowSize origin =
    let
        -- zoom =
        --     Z13
        viewAzimuth =
            270

        viewElevation =
            35

        viewArgs : ViewArgs
        viewArgs =
            { focalPoint = Point3d.origin
            , azimuth = Angle.degrees viewAzimuth
            , elevation = Angle.degrees viewElevation
            , distance = Length.meters 25000
            }

        model =
            { windowSize = windowSize
            , viewArgs = viewArgs

            -- , origin = origin
            , mapFrame = mercatorFrame origin
            , mercatorRate = mercatorRate (Tuple.first origin)
            , dragState = Static
            , loadedTiles = Dict.empty
            , displayedTiles = []
            }

        -- tiles =
        --     makeTiles
        --         (isInView viewArgs windowSize)
        --         model.mapFrame
        --         model.mercatorRate
        --         (Point3d.projectInto xyPlane viewArgs.focalPoint)
        --         (pickZoom model)
        --     |> List.map Tuple.first
        -- zoom =
        --     pickZoom (Tuple.first origin) windowSize viewArgs
        -- tiles =
        --     Maybe.map
        --         (fillTiles (isInView viewArgs windowSize) origin)
        --         zoom
        --         |> Maybe.withDefault []
    in
    updateTiles model



-- ( model, loadMissingTilesCmd model.loadedTiles tiles)
-- updateTiles model
-- |> Maybe.withDefault ( model, Cmd.none )


type Msg
    = TileLoaded TileKey (Maybe (Material.Texture Color))
    | LoadElevations (List TileKey) (AsyncOperationStatus (ApiResult (List (List Int))))
    | DragStart ( Float, Float )
    | DragMove Bool ( Float, Float )
    | DragStop ( Float, Float )
    | ZoomChanged WheelEvent


screenRectangle : WindowSize -> Rectangle2d Pixels screenCoords
screenRectangle windowSize =
    Rectangle2d.from
        Point2d.origin
        (Point2d.pixels
            (toFloat windowSize.width)
            (toFloat windowSize.height)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileLoaded tileKey texture ->
            -- ( { model | loadedTiles = Dict.insert tileKey texture model.loadedTiles }
            -- , Cmd.none
            -- )
            ( { model
                | loadedTiles =
                    Dict.update
                        tileKey
                        (\data ->
                            case data of
                                Just val ->
                                    Just { val | texture = MaybeX.unwrap NotStarted Resolved texture }

                                Nothing ->
                                    Just { texture = MaybeX.unwrap NotStarted Resolved texture, mesh = NotStarted }
                        )
                        model.loadedTiles
              }
            , Cmd.none
            )

        LoadElevations tiles Started ->
            ( model, loadElevationsCmd (Finished >> LoadElevations tiles) tiles )

        LoadElevations tiles (Finished (Ok res)) ->
            let
                makeMesh ( tx, ty, zoom ) elevVals =
                    tileMesh
                        xyPlane
                        model.mapFrame
                        model.mercatorRate
                        elevVals
                        { x = tx, y = ty, zoom = zoomLevel zoom }

                updateTileData : TileKey -> List Int -> Maybe TileData -> Maybe TileData
                updateTileData ( tx, ty, zoom ) elevVals data =
                    case data of
                        Just val ->
                            Just { val | mesh = makeMesh ( tx, ty, zoom ) elevVals |> Resolved }

                        Nothing ->
                            Just { texture = NotStarted, mesh = makeMesh ( tx, ty, zoom ) elevVals |> Resolved }

                newLoadedTiles =
                    ListX.zip tiles res
                        |> List.foldl
                            (\( tk, pts ) -> Dict.update tk (updateTileData tk pts))
                            model.loadedTiles
            in
            ( { model | loadedTiles = newLoadedTiles }, Cmd.none )

        LoadElevations tiles (Finished (Err err)) ->
            ( model, Cmd.none )

        -- LoadElevations tiles (Finished (Err err)) ->
        --     ( model, Cmd.none )
        DragStart ( x, y ) ->
            let
                point =
                    Camera3d.ray
                        (camera model.viewArgs)
                        (screenRectangle model.windowSize)
                        (Point2d.pixels x y)
                        |> Axis3d.intersectionWithPlane Plane3d.xy
            in
            case point of
                Just p ->
                    ( { model | dragState = MovingFrom p }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DragMove isDown ( x, y ) ->
            let
                point =
                    Camera3d.ray
                        (camera model.viewArgs)
                        (screenRectangle model.windowSize)
                        (Point2d.pixels x y)
                        |> Axis3d.intersectionWithPlane Plane3d.xy

                delta : Point3d Meters WorldCoords -> Point3d Meters WorldCoords -> ( Length.Length, Length.Length )
                delta p1 p2 =
                    ( Point3d.xCoordinate p1 |> minus (Point3d.xCoordinate p2)
                    , Point3d.yCoordinate p1 |> minus (Point3d.yCoordinate p2)
                    )

                move p ( dx, dy ) =
                    Point3d.xyz
                        (Point3d.xCoordinate p |> minus dx)
                        (Point3d.yCoordinate p |> plus dy)
                        (Point3d.zCoordinate p)
            in
            case ( model.dragState, point ) of
                ( MovingFrom prevPoint, Just currPoint ) ->
                    let
                        d =
                            delta currPoint prevPoint

                        newPoint =
                            move
                                model.viewArgs.focalPoint
                                d

                        newModel =
                            { model
                                | dragState =
                                    if isDown then
                                        MovingFrom (move currPoint d)

                                    else
                                        Static
                                , viewArgs =
                                    withFocalPoint newPoint model.viewArgs
                            }

                        -- tiles =
                        --     makeTiles
                        --         (isInView newModel.viewArgs newModel.windowSize)
                        --         newModel.mapFrame
                        --         newModel.mercatorRate
                        --         (Point3d.projectInto xyPlane newModel.viewArgs.focalPoint)
                        --         (pickZoom newModel)
                        --     |> List.map Tuple.first
                    in
                    updateTiles newModel

                -- ( newModel, loadMissingTilesCmd newModel.loadedTiles tiles)
                _ ->
                    ( model, Cmd.none )

        DragStop ( x, y ) ->
            ( { model
                | dragState = Static
              }
            , Cmd.none
            )

        ZoomChanged wheelEvent ->
            let
                newModel =
                    { model
                        | viewArgs =
                            model.viewArgs
                                |> adjustViewDistance (Length.meters (wheelEvent.deltaY * 10))
                    }
            in
            updateTiles newModel


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Static ->
            Sub.none

        MovingFrom _ ->
            Sub.batch
                [ BE.onMouseMove (D.map2 DragMove decodeButtons decodePosition)
                , BE.onMouseUp (D.map DragStop decodePosition)
                ]


mapItemView : Model -> Map3dItem -> ( Svg Msg, Scene3d.Entity WorldCoords )
mapItemView model mapItem =
    let
        cmr =
            camera model.viewArgs

        screenRect =
            Rectangle2d.from
                Point2d.origin
                (Point2d.pixels
                    (toFloat model.windowSize.width)
                    (toFloat model.windowSize.height)
                )

        -- localLatitude =
        --     Tuple.first model.origin
        -- ( cx, cy ) =
        --     toMercatorWeb model.origin
        to3dPoint : GeoPoint -> Elevation -> Point3d Meters WorldCoords
        to3dPoint p (ElevationMeters elev) =
            toMercatorPoint p
                |> Point2d.placeIn model.mapFrame
                |> Point2d.at model.mercatorRate
                |> Point3d.on xyPlane
                |> Point3d.translateIn Direction3d.positiveZ (Length.meters elev)

        project3dPoint : Point3d Meters WorldCoords -> ( Float, Float )
        project3dPoint p3d =
            Projection.toScreenSpace
                cmr
                screenRect
                p3d
                |> Point2d.toTuple Pixels.inPixels
                |> Tuple.mapSecond (\n -> toFloat model.windowSize.height - n)
    in
    case mapItem of
        Point p elev ->
            -- Scene3d.nothing
            ( Svg.g [] [], Scene3d.nothing )

        Marker id p elev ->
            let
                ( pProjX, pProjY ) =
                    to3dPoint p elev |> project3dPoint

                ( pgProjX, pgProjY ) =
                    to3dPoint p (ElevationMeters 0) |> project3dPoint
            in
            ( Svg.g []
                [ Svg.line
                    [ SvgAttr.strokeWidth "1"
                    , SvgAttr.stroke "#D7BDE2"
                    , SvgAttr.x1 <| String.fromFloat pProjX
                    , SvgAttr.y1 <| String.fromFloat pProjY
                    , SvgAttr.x2 <| String.fromFloat pgProjX
                    , SvgAttr.y2 <| String.fromFloat pgProjY
                    ]
                    []
                , Svg.circle
                    [ SvgAttr.fill "#34495E"
                    , SvgAttr.cx (String.fromFloat pProjX)
                    , SvgAttr.cy (String.fromFloat pProjY)
                    , SvgAttr.r "3"
                    ]
                    []
                , Svg.text_
                    [ SvgAttr.x <| String.fromFloat pProjX
                    , SvgAttr.y <| String.fromFloat pProjY
                    , SvgAttr.dy "-10"
                    , SvgAttr.textAnchor "middle"
                    , SvgAttr.fill "#34495E"
                    , SvgAttr.fontFamily "Roboto Mono"
                    ]
                    [ Svg.text id ]
                ]
              -- , Scene3d.lineSegment
              --     (Material.color Color.lightBlue)
              --     (LineSegment3d.from
              --         (Point3d.meters px py elev)
              --         (Point3d.meters px py 0)
              --     )
            , Scene3d.nothing
            )

        Line xs ->
            let
                coordsToString ( a, b ) =
                    String.fromFloat a ++ "," ++ String.fromFloat b

                pts =
                    xs |> List.map (uncurry to3dPoint >> project3dPoint >> coordsToString)

                -- to3dLineSegment ( p1, elev1 ) ( p2, elev2 ) =
                --     Scene3d.lineSegment
                --         (Material.color Color.black)
                --         (LineSegment3d.from
                --             (to3dPoint p1 elev1)
                --             (to3dPoint p2 elev2)
                --         )
                -- lineSegments =
                --     case xs of
                --         a :: b :: rest ->
                --             List.foldl
                --                 (\curr ( segments, prev ) -> ( to3dLineSegment prev curr :: segments, curr ))
                --                 ( [ to3dLineSegment a b ], b )
                --                 rest
                --                 |> Tuple.first
                --         _ ->
                --             []
            in
            ( Svg.polyline
                [ SvgAttr.points <| String.join " " pts
                , SvgAttr.strokeWidth "2"
                , SvgAttr.stroke "#555555"
                , SvgAttr.fill "transparent"
                ]
                []
              -- ( Svg.g [] []
              -- , Scene3d.group lineSegments
            , Scene3d.nothing
            )

        -- Scene3d.quad
        --     (Material.color (Color.rgba 0 0 0 0.2))
        --     (Point3d.meters -1000 -1000 0)
        --     (Point3d.meters 1000 -1000 0)
        --     (Point3d.meters 1000 1000 0)
        --     (Point3d.meters -1000 1000 0)
        -- Scene3d.point
        --     { radius = Quantity 5 }
        --     (Material.color (Color.rgba 0 0 0 0.2))
        --     (Point3d.meters 100 100 100)
        Cylinder p (DistanceMeters r) (ElevationMeters elev) ->
            ( Svg.g [] [], Scene3d.nothing )



-- Scene3d.cylinder
--     (Material.color (Color.rgba 0 0 0 0.2))
--     (Cylinder3d.centeredOn
--         (Point3d.meters 0 0 0)
--         Direction3d.positiveZ
--         { radius = Quantity r
--         , length = Quantity elev
--         }
--     )
-- Scene3d.point
--     { radius = Quantity 5 }
--     (Material.color (Color.rgba 0 0 0 0.2))
--     (Point3d.meters 100 100 100)


debugInfo : Model -> List String
debugInfo model =
    let
        n =
            0

        showGeoPoint ( LatitudeDegrees lat, LongitudeDegrees lon ) =
            String.fromFloat lat ++ ", " ++ String.fromFloat lon

        show2dPoint fromQty p =
            Point2d.toTuple fromQty p
                |> (\( x, y ) -> String.fromFloat x ++ ", " ++ String.fromFloat y)

        show3dPoint fromQty p =
            Point3d.toTuple fromQty p
                |> (\( x, y, z ) -> String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z)

        -- (Quantity depth) =
        --     Projection.depth (camera (Point3d.meters 0 0 0)
        -- ( ( Quantity fromX, Quantity fromY ), ( Quantity toX, Quantity toY ) ) =
        --     Rectangle2d.vertices screenRectangle
        --         |> List.filterMap
        --             (Camera3d.ray model.camera screenRectangle
        --                 >> Axis3d.intersectionWithPlane Plane3d.xy
        --                 >> Maybe.map (\p -> ( Point3d.xCoordinate p, Point3d.yCoordinate p ))
        --             )
        --         |> List.foldr
        --             (\( x, y ) ( ( minx, miny ), ( maxx, maxy ) ) ->
        --                 ( ( Quantity.min x minx, Quantity.min y miny )
        --                 , ( Quantity.max x maxx, Quantity.max y maxy )
        --                 )
        --             )
        --             ( ( Length.meters 0, Length.meters 0 )
        --             , ( Length.meters 0, Length.meters 0 )
        --             )
    in
    [-- model.displayedTiles |> List.length |> String.fromInt |> (++) "Tiles in view: "
     -- , showGeoPoint model.origin
     -- , toMercatorPoint model.origin |> show2dPoint getMercatorUnit
     -- , toMercatorPoint model.origin |> Point2d.placeIn model.mapFrame |> show2dPoint getMercatorUnit
     -- , "( ("
     --     ++ String.fromFloat fromX
     --     ++ ", "
     --     ++ String.fromFloat fromY
     --     ++ "), ("
     --     ++ String.fromFloat toX
     --     ++ ", "
     --     ++ String.fromFloat toY
     --     ++ ") )"
     -- , model.displayedTiles
     --     |> List.map
     --         (\( ( x, y, z ), ( p0, p1 ) ) ->
     --             String.fromInt x
     --                 ++ "/"
     --                 ++ String.fromInt y
     --                 ++ "/"
     --                 ++ String.fromInt z
     --                 ++ " "
     --                 ++ (if isInView model.camera model.windowSize ( 0, 0 ) then
     --                         "True"
     --                     else
     --                         "False"
     --                    )
     --         )
     --     |> String.join "\n"
     -- , "zoom: " ++ String.fromInt model.zoom
     -- , Maybe.Extra.unwrap "" (\(DistanceMeters x) -> String.fromFloat x) (tileLength model.zoom)
    ]



-- ++ tls


view : List Map3dItem -> Model -> Html Msg
view mapItems model =
    let
        -- camera : Camera3d Meters coordinates
        -- camera =
        --     Camera3d.perspective
        --         { viewpoint =
        --             -- Viewpoint3d.lookAt
        --             --     { eyePoint = Point3d.meters 5000 2000 3000
        --             --     , focalPoint = Point3d.origin
        --             --     , upDirection = Direction3d.positiveZ
        --             --     }
        --             Viewpoint3d.orbitZ
        --                 { focalPoint = Point3d.origin
        --                 , azimuth = Angle.degrees model.viewAzimuth
        --                 , elevation = Angle.degrees model.viewElevation
        --                 , distance = Length.meters 20000
        --                 }
        --         , verticalFieldOfView = Angle.degrees 30
        --         }
        -- center =
        --     toMercatorWeb model.origin
        -- pt =
        --     Scene3d.point
        --         { radius = Quantity 5 }
        --         (Material.color (Color.rgba 0 0 0 0.2))
        --         (Point3d.meters 0 0 0)
        -- tileUrl =
        --     "https://tile.openstreetmap.org/10/625/341.png"
        -- base =
        --     Scene3d.quad
        --         (Material.color Color.lightGray)
        --         (Point3d.meters -1000 -1000 0)
        --         (Point3d.meters 1000 -1000 0)
        --         (Point3d.meters 1000 1000 0)
        --         (Point3d.meters -1000 1000 0)
        unwrapTexture =
            deferredToMaybe
                >> MaybeX.unwrap
                    (Material.color Color.lightGray)
                    Material.texturedColor

        toTile ( tk, ( p0, p1 ) ) =
            Dict.get tk model.loadedTiles
                |> Maybe.withDefault
                    { texture = NotStarted
                    , mesh = NotStarted
                    }
                |> (\data ->
                        case deferredToMaybe data.mesh of
                            Just mesh ->
                                Scene3d.mesh (unwrapTexture data.texture) mesh

                            Nothing ->
                                Scene3d.quad
                                    (unwrapTexture data.texture)
                                    -- usually quad vertices are defined in counter-clockwise order
                                    -- but since in mercator projection y axis is inverted,
                                    -- we need flip the texture upside down by changing the order of vertices
                                    (Point3d.xyz (Point2d.xCoordinate p0) (Point2d.yCoordinate p1) (Length.meters 0))
                                    (Point3d.xyz (Point2d.xCoordinate p1) (Point2d.yCoordinate p1) (Length.meters 0))
                                    (Point3d.xyz (Point2d.xCoordinate p1) (Point2d.yCoordinate p0) (Length.meters 0))
                                    (Point3d.xyz (Point2d.xCoordinate p0) (Point2d.yCoordinate p0) (Length.meters 0))
                   )

        -- Scene3d.quad
        --     (Dict.get tk model.loadedTiles
        --         |> Maybe.map (\data -> data.texture)
        --         |> Maybe.andThen (Maybe.map Material.texturedColor)
        --         |> Maybe.withDefault (Material.color Color.lightGray)
        --     )
        --     -- usually quad vertices are defined in counter-clockwise order
        --     -- but since in mercator projection y axis is inverted,
        --     -- we need flip the texture upside down by changing the order of vertices
        --     (Point3d.xyz (Point2d.xCoordinate p0) (Point2d.yCoordinate p1) (Length.meters 0))
        --     (Point3d.xyz (Point2d.xCoordinate p1) (Point2d.yCoordinate p1) (Length.meters 0))
        --     (Point3d.xyz (Point2d.xCoordinate p1) (Point2d.yCoordinate p0) (Length.meters 0))
        --     (Point3d.xyz (Point2d.xCoordinate p0) (Point2d.yCoordinate p0) (Length.meters 0))
        -- continue: move to init
        base =
            Scene3d.group <|
                List.map toTile model.displayedTiles

        mis =
            List.map (mapItemView model) mapItems

        entities =
            base :: List.map Tuple.second mis

        svgs =
            List.map Tuple.first mis

        --List.map (mapItemView model) mapItems
        -- List.map mapItemView mapItems
    in
    div
        [ on "mousedown" (D.map DragStart decodePosition)
        , on "wheel" (D.map ZoomChanged decodeWheelEvent)
        , style "position" "relative"
        ]
        [ Scene3d.cloudy
            { entities = entities
            , upDirection = Direction3d.positiveZ
            , camera = camera model.viewArgs
            , clipDepth = Length.meters 1
            , background = Scene3d.transparentBackground
            , dimensions = ( Pixels.pixels model.windowSize.width, Pixels.pixels model.windowSize.height )
            }
        , Svg.svg
            [ SvgAttr.width (String.fromInt model.windowSize.width)
            , SvgAttr.height (String.fromInt model.windowSize.height)
            , SvgAttr.viewBox (String.join " " [ "0", "0", String.fromInt model.windowSize.width, String.fromInt model.windowSize.height ])
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            ]
            svgs
        ]


decodeButtons : D.Decoder Bool
decodeButtons =
    D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)


decodePosition : D.Decoder ( Float, Float )
decodePosition =
    D.map2 Tuple.pair
        (D.field "pageX" D.float)
        (D.field "pageY" D.float)


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    , offsetX : Float
    , offsetY : Float
    }


decodeWheelEvent : D.Decoder WheelEvent
decodeWheelEvent =
    D.map4
        WheelEvent
        (D.field "deltaX" D.float)
        (D.field "deltaY" D.float)
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)
