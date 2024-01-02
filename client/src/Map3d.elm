module Map3d exposing
    ( DragState(..)
    , Model
    , Msg(..)
    , TileData
    , ViewArgs
    , WheelEvent
    , init
    , subscriptions
    , update
    , view
    )

import Angle exposing (Angle)
import Api.Types exposing (..)
import Array exposing (Array)
import Axis3d exposing (at_)
import Basics.Extra exposing (uncurry)
import Browser.Events as BrowserEvents
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Common.ApiCommands exposing (hydrateTile, loadElevationTileCmd)
import Common.ApiResult exposing (ApiResult, DeferredResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredToMaybe, setPending)
import Constants exposing (earthCircumference)
import Dict exposing (Dict)
import Dict.Extra as DictX
import Direction3d
import Domain.GeoUtils exposing (degreesLatitude, degreesLongitude)
import Flags exposing (WindowSize)
import Frame2d exposing (Frame2d)
import Html exposing (Html, div, input, text)
import Html.Attributes as HtmlAttr exposing (attribute, style, type_)
import Html.Events as HtmlEvents exposing (on)
import Json.Decode as D
import Length exposing (Length, Meters)
import Map3dUtils exposing (Map3dItem(..), MercatorCoords, MercatorUnit, PlaneCoords(..), WorldCoords, fromMercatorPoint, getMercatorUnit, makeMesh_, makeTilePoints, makeTiles, mercatorFrame, mercatorRate, tileMesh, tileRectangle, toMercatorPoint)
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
import Tile exposing (TileKey, ZoomLevel, fromTileKey, tileKeyToUrl, zoomLevel)
import Viewpoint3d


type WebGLResult
    = Web


type DragState
    = MovingFrom { azimuth : Angle, elevation : Angle, point : Point3d Meters WorldCoords, xy : ( Float, Float ) }
    | Static


type alias ViewArgs =
    { focalPoint : Point3d Meters WorldCoords -- the center point we're looking at
    , azimuth : Angle -- camera angle around the z axis
    , elevation : Angle -- camera angle relative to xy plane
    , distance : Quantity Float Meters -- camera distance from the focal point
    }


type alias TileData =
    { texture : Deferred (Material.Texture Color)
    , mesh : Deferred (Mesh.Textured WorldCoords) -- todo: add Result to support refetching in case of error
    }


type alias Model =
    { windowSize : WindowSize
    , dragControlsAzimuthAndElevation : Bool
    , mapFrame : Frame2d MercatorUnit PlaneCoords { defines : MercatorCoords }
    , mercatorRate : Quantity Float (Rate Meters MercatorUnit)
    , dragState : DragState
    , viewArgs : ViewArgs
    , loadedTiles : Dict TileKey TileData
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


withAzimuthElevation : Angle -> Angle -> ViewArgs -> ViewArgs
withAzimuthElevation azimuth elevation viewArgs =
    { viewArgs | azimuth = azimuth, elevation = elevation }


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
        latRad =
            SketchPlane3d.originPoint xyPlane
                |> Point3d.projectInto xyPlane
                |> Point2d.at_ model.mercatorRate
                |> Point2d.relativeTo model.mapFrame
                |> fromMercatorPoint
                |> .lat
                |> degreesLatitude
                |> degrees

        viewDistance =
            Length.inMeters model.viewArgs.distance
    in
    logBase 2 (earthCircumference * cos latRad / viewDistance)
        |> round
        |> zoomLevel


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

        letRad =
            SketchPlane3d.originPoint xyPlane
                |> Point3d.projectInto xyPlane
                |> Point2d.at_ model.mercatorRate
                |> Point2d.relativeTo model.mapFrame
                |> fromMercatorPoint
                |> .lat
                |> degreesLatitude
                |> degrees

        zoom =
            Maybe.map ((\l -> logBase 2 (earthCircumference * cos letRad / l)) >> round >> zoomLevel) tLen

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

        _ =
            tiles |> List.map Tuple.first

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
            let
                loadTileElevationsCmds =
                    missingMeshes |> List.map (\tileKey -> loadElevationTileCmd (ElevationsTileLoaded tileKey) tileKey)
            in
            Cmd.batch loadTileElevationsCmds
                :: (missingTextures |> List.map loadTextureCmd)
    in
    ( newModel, Cmd.batch cmds )


camera : ViewArgs -> Camera3d Meters WorldCoords
camera viewArgs =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.orbitZ viewArgs
        , viewportHeight = viewArgs.distance
        }


isInView : ViewArgs -> WindowSize -> Point2d Meters PlaneCoords -> Bool
isInView viewArgs windowSize p =
    let
        p3d =
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
            , dragControlsAzimuthAndElevation = False
            , viewArgs = viewArgs
            , mapFrame = mercatorFrame origin
            , mercatorRate = mercatorRate origin.lat
            , dragState = Static
            , loadedTiles = Dict.empty
            , displayedTiles = []
            }
    in
    updateTiles model


type Msg
    = SetDragControlAzimuthAndElevation Bool
    | TileLoaded TileKey (Maybe (Material.Texture Color))
    | ElevationsTileLoaded TileKey (ApiResult ElevationPointsTile)
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
        SetDragControlAzimuthAndElevation b ->
            ( { model | dragControlsAzimuthAndElevation = b }, Cmd.none )

        TileLoaded tileKey texture ->
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

        ElevationsTileLoaded tileKey (Ok res) ->
            let
                updateTileData : ElevationPointsTile -> Maybe TileData -> Maybe TileData
                updateTileData elevVals data =
                    case data of
                        Just val ->
                            Just { val | mesh = makeMesh_ model.mapFrame model.mercatorRate xyPlane elevVals |> Resolved }

                        Nothing ->
                            Just { texture = NotStarted, mesh = makeMesh_ model.mapFrame model.mercatorRate xyPlane elevVals |> Resolved }
            in
            ( { model | loadedTiles = Dict.update tileKey (updateTileData res) model.loadedTiles }, Cmd.none )

        ElevationsTileLoaded _ (Err _) ->
            ( model, Cmd.none )

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
                    ( { model | dragState = MovingFrom { azimuth = model.viewArgs.azimuth, elevation = model.viewArgs.elevation, point = p, xy = ( x, y ) } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DragMove isDown ( x, y ) ->
            let
                projectedPoint =
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
            case ( model.dragState, projectedPoint, model.dragControlsAzimuthAndElevation ) of
                ( MovingFrom { azimuth, elevation, point }, Just currPoint, False ) ->
                    let
                        d =
                            delta currPoint point

                        newPoint =
                            move
                                model.viewArgs.focalPoint
                                d

                        newModel =
                            { model
                                | dragState =
                                    if isDown then
                                        MovingFrom { azimuth = azimuth, elevation = elevation, point = move currPoint d, xy = ( x, y ) }

                                    else
                                        Static
                                , viewArgs =
                                    withFocalPoint newPoint model.viewArgs
                            }
                    in
                    updateTiles newModel

                ( MovingFrom { azimuth, elevation, xy }, Just currPoint, True ) ->
                    let
                        ( lastX, lastY ) =
                            xy

                        newAzimuth =
                            Angle.degrees (Angle.inDegrees azimuth + ((lastX - x) * 0.1))

                        newElevation =
                            Angle.degrees (Angle.inDegrees elevation + ((y - lastY) * 0.1))
                    in
                    ( { model
                        | dragState = MovingFrom { azimuth = newAzimuth, elevation = newElevation, point = currPoint, xy = ( x, y ) }
                        , viewArgs = withAzimuthElevation newAzimuth newElevation model.viewArgs
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DragStop _ ->
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
    let
        isShiftKey : String -> Bool
        isShiftKey =
            (==) "Shift"

        keyModSub =
            Sub.batch
                [ BrowserEvents.onKeyDown (D.map (isShiftKey >> SetDragControlAzimuthAndElevation) decodeKey)
                , BrowserEvents.onKeyUp (D.map (isShiftKey >> not >> SetDragControlAzimuthAndElevation) decodeKey)
                ]

        mouseSub =
            case model.dragState of
                Static ->
                    Sub.none

                MovingFrom _ ->
                    Sub.batch
                        [ BrowserEvents.onMouseMove (D.map2 DragMove decodeMouseButtons decodePosition)
                        , BrowserEvents.onMouseUp (D.map DragStop decodePosition)
                        ]
    in
    Sub.batch [ keyModSub, mouseSub ]


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
            , Scene3d.nothing
            )

        Line xs ->
            let
                coordsToString ( a, b ) =
                    String.fromFloat a ++ "," ++ String.fromFloat b

                pts =
                    xs |> List.map (uncurry to3dPoint >> project3dPoint >> coordsToString)
            in
            ( Svg.polyline
                [ SvgAttr.points <| String.join " " pts
                , SvgAttr.strokeWidth "2"
                , SvgAttr.stroke "#555555"
                , SvgAttr.fill "transparent"
                ]
                []
            , Scene3d.nothing
            )

        Cylinder p (DistanceMeters r) (ElevationMeters elev) ->
            ( Svg.g [] [], Scene3d.nothing )


view : List Map3dItem -> Model -> Html Msg
view mapItems model =
    let
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

        base =
            Scene3d.group <|
                List.map toTile model.displayedTiles

        mis =
            List.map (mapItemView model) mapItems

        entities =
            base :: List.map Tuple.second mis

        svgs =
            List.map Tuple.first mis
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


decodeMouseButtons : D.Decoder Bool
decodeMouseButtons =
    D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)


decodePosition : D.Decoder ( Float, Float )
decodePosition =
    D.map2 Tuple.pair
        (D.field "pageX" D.float)
        (D.field "pageY" D.float)


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


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
