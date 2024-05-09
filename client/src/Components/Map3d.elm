module Components.Map3d exposing
    ( DemoState(..)
    , DemoType(..)
    , Model
    , Msg(..)
    , TileData
    , WheelEvent
    , camera
    , init
    , screenRectangle
    , subscriptions
    , update
    , view
    )

import Angle exposing (Angle)
import Api.Types exposing (..)
import Axis3d
import Basics.Extra exposing (uncurry)
import Browser.Events as BrowserEvents
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Common.ApiCommands exposing (loadElevationTileCmd)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (Deferred(..), deferredToMaybe, setPending)
import Components.Map3dUtils
    exposing
        ( Map3dItem(..)
        , MercatorCoords
        , MercatorUnit
        , PlaneCoords
        , ViewArgs
        , WorldCoords
        , fromMercatorPoint
        , makeMesh
        , makeTiles
        , mercatorFrame
        , mercatorRate
        , toMercatorPoint
        )
import Components.Player exposing (Msg)
import Constants exposing (earthCircumference)
import Dict exposing (Dict)
import Direction3d
import Domain.GeoUtils exposing (degreesLatitude)
import Flags exposing (WindowSize)
import Frame2d exposing (Frame2d)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as D
import Length exposing (Length, Meters)
import List.Extra
import Maybe.Extra as MaybeX
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Projection
import Quantity exposing (Quantity, Rate)
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d exposing (SketchPlane3d)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task
import Tile exposing (TileKey, ZoomLevel, tileKeyToUrl, zoomLevel)
import Time
import Viewpoint3d


type alias ViewMovementStep =
    { finalView : ViewArgs, durationMillis : Int }


type DemoType
    = DragDemo
    | RotateDemo
    | ZoomDemo
    | ComplexDemo


makePoint : Model -> Quantity Float Meters -> GeoPoint -> Point3d Meters WorldCoords
makePoint model elev =
    toMercatorPoint
        >> Point2d.placeIn model.mapFrame
        >> Point2d.at model.mercatorRate
        >> Point3d.on xyPlane
        >> Point3d.translateIn Direction3d.positiveZ elev


dragDemoSteps : Model -> List ViewMovementStep
dragDemoSteps model =
    let
        initialFocalPoint : Point3d Meters WorldCoords
        initialFocalPoint =
            model.viewArgs.focalPoint

        movementAxis : Axis3d.Axis3d Meters WorldCoords
        movementAxis =
            Axis3d.through initialFocalPoint Direction3d.positiveX
    in
    [ { finalView =
            { focalPoint = Point3d.along movementAxis (Length.meters 5000)
            , azimuth = model.viewArgs.azimuth
            , elevation = model.viewArgs.elevation
            , distance = model.viewArgs.distance
            }
      , durationMillis = 3000
      }
    , { finalView = model.viewArgs
      , durationMillis = 3000
      }
    ]


rotateDemoSteps : Model -> List ViewMovementStep
rotateDemoSteps model =
    [ { finalView =
            { focalPoint = model.viewArgs.focalPoint
            , azimuth = Angle.degrees 180
            , elevation = model.viewArgs.elevation
            , distance = model.viewArgs.distance
            }
      , durationMillis = 3000
      }
    , { finalView = model.viewArgs
      , durationMillis = 3000
      }
    ]


zoomDemoSteps : Model -> List ViewMovementStep
zoomDemoSteps model =
    [ { finalView =
            { focalPoint = model.viewArgs.focalPoint
            , azimuth = model.viewArgs.azimuth
            , elevation = model.viewArgs.elevation
            , distance = Length.meters 10000
            }
      , durationMillis = 3000
      }
    , { finalView = model.viewArgs
      , durationMillis = 3000
      }
    ]


complexDemoSteps : Model -> List ViewMovementStep
complexDemoSteps model =
    [ { finalView =
            { focalPoint =
                makePoint
                    model
                    (Length.meters 500)
                    { lat = LatitudeDegrees 45.208451, lon = LongitudeDegrees 5.726031 }
            , azimuth = Angle.degrees 220
            , elevation = Angle.degrees 35
            , distance = Length.meters 25000
            }
      , durationMillis = 3000
      }
    , { finalView =
            { focalPoint =
                makePoint
                    model
                    (Length.meters 500)
                    { lat = LatitudeDegrees 45.208451, lon = LongitudeDegrees 5.726031 }
            , azimuth = Angle.degrees 220
            , elevation = Angle.degrees 35
            , distance = Length.meters 15000
            }
      , durationMillis = 3000
      }
    , { finalView =
            { focalPoint =
                makePoint
                    model
                    (Length.meters 500)
                    { lat = LatitudeDegrees 45.269101, lon = LongitudeDegrees 5.849835 }
            , azimuth = Angle.degrees 220
            , elevation = Angle.degrees 35
            , distance = Length.meters 10000
            }
      , durationMillis = 3000
      }
    ]


makeFrames : Int -> ViewArgs -> ViewArgs -> List ViewArgs
makeFrames numTicks initialView finalView =
    let
        azimuthTick : Angle
        azimuthTick =
            (finalView.azimuth |> Quantity.minus initialView.azimuth) |> Quantity.divideBy (toFloat numTicks)

        elevationTick : Angle
        elevationTick =
            (finalView.elevation |> Quantity.minus initialView.elevation) |> Quantity.divideBy (toFloat numTicks)

        distanceTick : Length
        distanceTick =
            (finalView.distance |> Quantity.minus initialView.distance) |> Quantity.divideBy (toFloat numTicks)

        interpolateFocalPointTick : Int -> Point3d Meters WorldCoords
        interpolateFocalPointTick tick =
            Point3d.interpolateFrom initialView.focalPoint finalView.focalPoint (toFloat tick / toFloat numTicks)

        updTick : Int -> ViewArgs
        updTick tick =
            { initialView
                | azimuth = initialView.azimuth |> Quantity.plus (azimuthTick |> Quantity.multiplyBy (toFloat tick))
                , elevation = initialView.elevation |> Quantity.plus (elevationTick |> Quantity.multiplyBy (toFloat tick))
                , distance = initialView.distance |> Quantity.plus (distanceTick |> Quantity.multiplyBy (toFloat tick))
                , focalPoint = interpolateFocalPointTick tick
            }
    in
    List.range 1 (max 1 numTicks)
        |> List.map updTick


demoFrames : ViewArgs -> List ViewMovementStep -> List ViewArgs -> List ViewArgs
demoFrames currentView stepsToGo readyFrames =
    case stepsToGo of
        [] ->
            readyFrames

        nextStep :: remainingSteps ->
            demoFrames nextStep.finalView remainingSteps (readyFrames ++ makeFrames (nextStep.durationMillis // 10) currentView nextStep.finalView)


demoFrames_ : Model -> List ViewMovementStep -> List ViewArgs
demoFrames_ model steps =
    demoFrames model.viewArgs steps []


type DragState
    = MovingFrom { azimuth : Angle, elevation : Angle, xy : ( Float, Float ) }
    | Static


type DemoState
    = DemoNotStarted
    | DemoInProgress (List ViewArgs)


type alias TileData =
    { texture : Deferred (Material.Texture Color)
    , mesh : Deferred ( Mesh.Textured WorldCoords, Mesh.Shadow WorldCoords )
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
    , cursorPosition : Maybe ( Float, Float )
    , demoState : DemoState
    }


withPendingTextures : List TileKey -> Model -> Model
withPendingTextures keys model =
    let
        setPendingTexture : Maybe TileData -> Maybe TileData
        setPendingTexture item =
            case item of
                Just data ->
                    Just { data | texture = setPending data.texture }

                Nothing ->
                    Just { texture = InProgress, mesh = NotStarted }

        updatedTiles : Dict TileKey TileData
        updatedTiles =
            keys
                |> List.foldl (\key dict -> Dict.update key setPendingTexture dict) model.loadedTiles
    in
    { model | loadedTiles = updatedTiles }


withPendingMeshes : List TileKey -> Model -> Model
withPendingMeshes keys model =
    let
        setPendingMesh : Maybe TileData -> Maybe TileData
        setPendingMesh item =
            case item of
                Just data ->
                    Just { data | mesh = setPending data.mesh }

                Nothing ->
                    Just { texture = NotStarted, mesh = InProgress }

        updatedTiles : Dict TileKey TileData
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
        newDistance : Quantity Float Meters
        newDistance =
            viewArgs.distance |> Quantity.plus delta

        minDistance : Length
        minDistance =
            Length.meters 3000

        maxDistance : Length
        maxDistance =
            Length.meters 40000
    in
    { viewArgs | distance = Quantity.clamp minDistance maxDistance newDistance }


xyPlane : SketchPlane3d Meters WorldCoords { defines : PlaneCoords }
xyPlane =
    SketchPlane3d.xy


loadTextureCmd : TileKey -> Cmd Msg
loadTextureCmd tileKey =
    tileKey
        |> tileKeyToUrl
        |> Material.load
        |> Task.attempt (Result.toMaybe >> TileLoaded tileKey)


updateTiles : Model -> ( Model, Cmd Msg )
updateTiles model =
    let
        numTiles : Int
        numTiles =
            round (toFloat model.windowSize.width / 256)

        cmr : Camera3d Meters WorldCoords
        cmr =
            camera model.viewArgs

        sRect : Rectangle2d Pixels screenCoords
        sRect =
            screenRectangle model.windowSize

        pLeft : Maybe (Point3d Meters WorldCoords)
        pLeft =
            Camera3d.ray cmr sRect (Point2d.pixels 0 0)
                |> Axis3d.intersectionWithPlane Plane3d.xy

        pRight : Maybe (Point3d Meters WorldCoords)
        pRight =
            Camera3d.ray cmr sRect (Point2d.pixels (toFloat model.windowSize.width) 0)
                |> Axis3d.intersectionWithPlane Plane3d.xy

        dist : Maybe (Quantity Float Meters)
        dist =
            Maybe.map2 Point3d.distanceFrom pLeft pRight

        tLen : Maybe Float
        tLen =
            Maybe.map (Length.inMeters >> (\d -> d / toFloat numTiles)) dist

        letRad : Float
        letRad =
            SketchPlane3d.originPoint xyPlane
                |> Point3d.projectInto xyPlane
                |> Point2d.at_ model.mercatorRate
                |> Point2d.relativeTo model.mapFrame
                |> fromMercatorPoint
                |> .lat
                |> degreesLatitude
                |> degrees

        zoom : Maybe ZoomLevel
        zoom =
            Maybe.map ((\l -> logBase 2 (earthCircumference * cos letRad / l)) >> round >> zoomLevel) tLen

        tiles : List ( TileKey, ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) )
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

        newLoadedTiles : Dict TileKey TileData
        newLoadedTiles =
            tiles
                |> List.filterMap (Tuple.first >> (\tk -> Dict.get tk model.loadedTiles |> Maybe.map (Tuple.pair tk)))
                |> Dict.fromList

        -- gets tile keys that are missing the given property
        missingKeys : (TileData -> Deferred a) -> List ( TileKey, b ) -> List TileKey
        missingKeys prop =
            List.filterMap
                (\( tk, _ ) ->
                    case Maybe.map prop (Dict.get tk newLoadedTiles) of
                        Just NotStarted ->
                            Just tk

                        Just _ ->
                            Nothing

                        Nothing ->
                            Just tk
                )

        missingTextures : List TileKey
        missingTextures =
            missingKeys .texture tiles

        missingMeshes : List TileKey
        missingMeshes =
            missingKeys .mesh tiles

        newModel : Model
        newModel =
            { model | displayedTiles = tiles, loadedTiles = newLoadedTiles }
                |> withPendingTextures missingTextures
                |> withPendingMeshes missingMeshes

        cmds : List (Cmd Msg)
        cmds =
            let
                loadTileElevationsCmds : List (Cmd Msg)
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
        p3d : Point3d Meters WorldCoords
        p3d =
            Point3d.on xyPlane p

        sRect : Rectangle2d Pixels screenCoords
        sRect =
            screenRectangle windowSize

        cmr : Camera3d Meters WorldCoords
        cmr =
            camera viewArgs

        projectedPoint : Point2d Pixels screenCoords
        projectedPoint =
            Projection.toScreenSpace
                cmr
                sRect
                p3d

        depth : Quantity Float Meters
        depth =
            Projection.depth cmr p3d
    in
    Rectangle2d.contains projectedPoint sRect
        && Quantity.greaterThan (Length.meters 0) depth
        && Quantity.lessThan (Length.meters 200000) depth


init : WindowSize -> GeoPoint -> ( Model, Cmd Msg )
init windowSize origin =
    let
        viewAzimuth : Float
        viewAzimuth =
            270

        viewElevation : Float
        viewElevation =
            50

        viewArgs : ViewArgs
        viewArgs =
            { focalPoint = Point3d.origin
            , azimuth = Angle.degrees viewAzimuth
            , elevation = Angle.degrees viewElevation
            , distance = Length.meters 25000
            }

        model : Model
        model =
            { windowSize = windowSize
            , dragControlsAzimuthAndElevation = False
            , viewArgs = viewArgs
            , mapFrame = mercatorFrame origin
            , mercatorRate = mercatorRate origin.lat
            , dragState = Static
            , loadedTiles = Dict.empty
            , displayedTiles = []
            , cursorPosition = Nothing
            , demoState = DemoNotStarted
            }
    in
    updateTiles model


type MouseButtonState
    = DownLeft
    | DownRight


type Msg
    = SetDragControlAzimuthAndElevation Bool
    | TileLoaded TileKey (Maybe (Material.Texture Color))
    | ElevationsTileLoaded TileKey (ApiResult ElevationPointsTile)
    | DragStart ( Float, Float )
    | DragMove MouseButtonState ( Float, Float )
    | DragStop ( Float, Float )
    | CursorMoved ( Float, Float )
    | ZoomChanged WheelEvent
    | DemoStarted DemoType
    | DemoTick Time.Posix
    | DemoFinished
    | ViewReset
    | PointFocused GeoPoint
    | NoOp


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
                            Just { val | mesh = makeMesh model.mapFrame model.mercatorRate xyPlane elevVals |> Resolved }

                        Nothing ->
                            Just { texture = NotStarted, mesh = makeMesh model.mapFrame model.mercatorRate xyPlane elevVals |> Resolved }
            in
            ( { model | loadedTiles = Dict.update tileKey (updateTileData res) model.loadedTiles }, Cmd.none )

        ElevationsTileLoaded _ (Err _) ->
            ( model, Cmd.none )

        DragStart ( x, y ) ->
            ( { model
                | dragState =
                    MovingFrom
                        { azimuth = model.viewArgs.azimuth
                        , elevation = model.viewArgs.elevation
                        , xy = ( x, y )
                        }
              }
            , Cmd.none
            )

        DragMove btnState ( x, y ) ->
            case ( model.dragState, btnState ) of
                ( MovingFrom { azimuth, elevation, xy }, DownLeft ) ->
                    let
                        projectedPoint : Maybe (Point3d Meters WorldCoords)
                        projectedPoint =
                            Camera3d.ray
                                (camera model.viewArgs)
                                (screenRectangle model.windowSize)
                                (Point2d.pixels x (toFloat model.windowSize.height - y))
                                |> Axis3d.intersectionWithPlane Plane3d.xy

                        projectedPrevPoint : Maybe (Point3d Meters WorldCoords)
                        projectedPrevPoint =
                            Camera3d.ray
                                (camera model.viewArgs)
                                (screenRectangle model.windowSize)
                                (Point2d.pixels (Tuple.first xy) (toFloat model.windowSize.height - Tuple.second xy))
                                |> Axis3d.intersectionWithPlane Plane3d.xy

                        direction : Maybe (Direction3d.Direction3d WorldCoords)
                        direction =
                            MaybeX.andThen2 (\from to -> Direction3d.from from to) projectedPoint projectedPrevPoint

                        distance : Maybe (Quantity Float Meters)
                        distance =
                            Maybe.map2 (\from to -> Point3d.distanceFrom from to) projectedPoint projectedPrevPoint

                        target : Maybe (Point3d Meters WorldCoords)
                        target =
                            Maybe.map2 (\dir dist -> Point3d.translateIn dir dist model.viewArgs.focalPoint) direction distance

                        newModel : Model
                        newModel =
                            { model
                                | dragState =
                                    MovingFrom { azimuth = azimuth, elevation = elevation, xy = ( x, y ) }
                                , viewArgs =
                                    target
                                        |> Maybe.map (\p -> withFocalPoint p model.viewArgs)
                                        |> Maybe.withDefault model.viewArgs
                            }
                    in
                    updateTiles newModel

                ( MovingFrom { azimuth, elevation, xy }, DownRight ) ->
                    let
                        ( lastX, lastY ) =
                            xy

                        newAzimuth : Angle
                        newAzimuth =
                            Angle.degrees (Angle.inDegrees azimuth + ((lastX - x) * 0.1))

                        newElevation : Angle
                        newElevation =
                            Angle.degrees (Angle.inDegrees elevation + ((y - lastY) * 0.1))

                        minAngle : Angle
                        minAngle =
                            Angle.degrees 20

                        maxAngle : Angle
                        maxAngle =
                            Angle.degrees 90
                    in
                    { model
                        | dragState = MovingFrom { azimuth = newAzimuth, elevation = Quantity.clamp minAngle maxAngle newElevation, xy = ( x, y ) }
                        , viewArgs = withAzimuthElevation newAzimuth newElevation model.viewArgs
                    }
                        |> updateTiles

                _ ->
                    ( model, Cmd.none )

        DragStop _ ->
            ( { model
                | dragState = Static
              }
            , Cmd.none
            )

        CursorMoved ( x, y ) ->
            ( { model | cursorPosition = Just ( x, y ) }, Cmd.none )

        ZoomChanged wheelEvent ->
            let
                newModel : Model
                newModel =
                    { model
                        | viewArgs =
                            model.viewArgs
                                |> adjustViewDistance (Length.meters (wheelEvent.deltaY * 10))
                    }
            in
            updateTiles newModel

        DemoStarted demoType ->
            let
                demoSteps : List ViewMovementStep
                demoSteps =
                    case demoType of
                        DragDemo ->
                            dragDemoSteps model

                        RotateDemo ->
                            rotateDemoSteps model

                        ZoomDemo ->
                            zoomDemoSteps model

                        ComplexDemo ->
                            complexDemoSteps model
            in
            ( { model
                | demoState =
                    demoSteps
                        |> demoFrames_ model
                        |> DemoInProgress
              }
            , Cmd.none
            )

        DemoTick _ ->
            case model.demoState of
                DemoInProgress (h :: t) ->
                    { model | viewArgs = h, demoState = DemoInProgress t } |> updateTiles

                DemoInProgress [] ->
                    ( { model | demoState = DemoNotStarted }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DemoFinished ->
            ( { model | demoState = DemoNotStarted }, Cmd.none )

        PointFocused point ->
            let
                viewArgs : ViewArgs
                viewArgs =
                    model.viewArgs

                focalPoint : Point3d Meters WorldCoords
                focalPoint =
                    makePoint model (Length.meters 500) point
            in
            { model
                | viewArgs = { viewArgs | focalPoint = focalPoint }
            }
                |> updateTiles

        ViewReset ->
            let
                newViewArgs : ViewArgs
                newViewArgs =
                    { focalPoint = model.viewArgs.focalPoint
                    , azimuth = Angle.degrees 270
                    , elevation = Angle.degrees 50
                    , distance = Length.meters 25000
                    }
            in
            { model | viewArgs = newViewArgs, demoState = DemoNotStarted }
                |> updateTiles

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        isShiftKey : String -> Bool
        isShiftKey =
            (==) "Shift"

        keyModSub : Sub Msg
        keyModSub =
            Sub.batch
                [ BrowserEvents.onKeyDown (D.map (isShiftKey >> SetDragControlAzimuthAndElevation) decodeKey)
                , BrowserEvents.onKeyUp (D.map (isShiftKey >> not >> SetDragControlAzimuthAndElevation) decodeKey)
                ]

        dragSub : Sub Msg
        dragSub =
            case model.dragState of
                Static ->
                    Sub.none

                MovingFrom _ ->
                    Sub.batch
                        [ BrowserEvents.onMouseMove (D.map2 DragMove decodeMouseButtons decodePosition)
                        , BrowserEvents.onMouseUp (D.map DragStop decodePosition)
                        ]

        cursorSub : Sub Msg
        cursorSub =
            BrowserEvents.onMouseMove (D.map CursorMoved decodePosition)

        demoSub : Sub Msg
        demoSub =
            case model.demoState of
                DemoNotStarted ->
                    Sub.none

                DemoInProgress _ ->
                    Time.every 10 DemoTick
    in
    Sub.batch [ keyModSub, dragSub, cursorSub, demoSub ]


mapItemView : Model -> Map3dItem -> ( Svg msg, Scene3d.Entity WorldCoords )
mapItemView model mapItem =
    let
        cmr : Camera3d Meters WorldCoords
        cmr =
            camera model.viewArgs

        screenRect : Rectangle2d Pixels coordinates
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
        Point geoPoint elevation ->
            let
                ( pProjX, pProjY ) =
                    to3dPoint geoPoint elevation |> project3dPoint
            in
            ( Svg.g []
                [ Svg.circle
                    [ SvgAttr.fill "#34495E"
                    , SvgAttr.cx (String.fromFloat pProjX)
                    , SvgAttr.cy (String.fromFloat pProjY)
                    , SvgAttr.r "3"
                    ]
                    []
                ]
            , Scene3d.nothing
            )

        Marker id p elev ->
            let
                ( pProjX, pProjY ) =
                    to3dPoint p elev |> project3dPoint
            in
            ( Svg.g []
                [ Svg.circle
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
                coordsToString : ( Float, Float ) -> String
                coordsToString ( a, b ) =
                    String.fromFloat a ++ "," ++ String.fromFloat b

                pts : List String
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

        Cylinder _ (DistanceMeters _) (ElevationMeters _) ->
            ( Svg.g [] [], Scene3d.nothing )


view : { restartOnboarding : msg, mapMsg : Msg -> msg } -> List Map3dItem -> Model -> Html msg
view { restartOnboarding, mapMsg } mapItems model =
    let
        unwrapTexture : Deferred (Material.Texture Color) -> Material.Material WorldCoords { a | uvs : () }
        unwrapTexture =
            deferredToMaybe
                >> MaybeX.unwrap
                    (Material.color Color.lightGray)
                    Material.texturedColor

        unwrapTexMat : Deferred (Material.Texture Color) -> Material.Material WorldCoords { a | normals : (), uvs : () }
        unwrapTexMat =
            deferredToMaybe
                >> MaybeX.unwrap
                    (Material.texturedNonmetal { baseColor = Material.constant Color.gray, roughness = Material.constant 0.5 })
                    (\texture -> Material.texturedNonmetal { baseColor = texture, roughness = Material.constant 0.5 })

        toTile : ( TileKey, ( Point2d Meters PlaneCoords, Point2d Meters PlaneCoords ) ) -> Scene3d.Entity WorldCoords
        toTile ( tk, ( p0, p1 ) ) =
            Dict.get tk model.loadedTiles
                |> Maybe.withDefault
                    { texture = NotStarted
                    , mesh = NotStarted
                    }
                |> (\data ->
                        case deferredToMaybe data.mesh of
                            Just ( mesh, shadow ) ->
                                -- Scene3d.meshWithShadow (Material.nonmetal { baseColor = Color.gray, roughness = 0.5 }) mesh shadow
                                Scene3d.meshWithShadow (unwrapTexMat data.texture) mesh shadow

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

        base : Scene3d.Entity WorldCoords
        base =
            Scene3d.group <|
                List.map toTile model.displayedTiles

        mis : List ( Svg msg, Scene3d.Entity WorldCoords )
        mis =
            List.map (mapItemView model) mapItems

        entities : List (Scene3d.Entity WorldCoords)
        entities =
            base :: List.map Tuple.second mis

        svgs : List (Svg msg)
        svgs =
            List.map Tuple.first mis
    in
    div
        [ on "mousedown" (D.map (DragStart >> mapMsg) decodePosition)
        , on "wheel" (D.map (ZoomChanged >> mapMsg) decodeWheelEvent)
        , style "position" "relative"
        , Html.Events.preventDefaultOn "contextmenu" (D.succeed ( mapMsg NoOp, True ))
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection = Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy (Angle.degrees 45) (Angle.degrees 45) |> Direction3d.reverse
            , shadows = True
            , dimensions = ( Pixels.pixels model.windowSize.width, Pixels.pixels model.windowSize.height )
            , camera = camera model.viewArgs
            , clipDepth = Length.meters 1
            , background = Scene3d.transparentBackground
            , entities = entities
            }

        --   Scene3d.cloudy
        --     { entities = entities
        --     , upDirection = Direction3d.positiveZ
        --     , camera = camera model.viewArgs
        --     , clipDepth = Length.meters 1
        --     , background = Scene3d.transparentBackground
        --     , dimensions = ( Pixels.pixels model.windowSize.width, Pixels.pixels model.windowSize.height )
        --     }
        , Svg.svg
            [ SvgAttr.width (String.fromInt model.windowSize.width)
            , SvgAttr.height (String.fromInt model.windowSize.height)
            , SvgAttr.viewBox (String.join " " [ "0", "0", String.fromInt model.windowSize.width, String.fromInt model.windowSize.height ])
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            ]
            svgs
        , button
            [ style "position" "absolute"
            , style "top" "20px"
            , style "right" "20px"
            , style "width" "30px"
            , style "height" "30px"
            , style "border-radius" "30px"
            , onClick restartOnboarding
            ]
            [ text "R" ]
        ]


mouseButtonFromInt : Int -> D.Decoder MouseButtonState
mouseButtonFromInt n =
    case n of
        1 ->
            D.succeed DownLeft

        2 ->
            D.succeed DownRight

        _ ->
            D.fail "Invalid mouse button"


decodeMouseButtons : D.Decoder MouseButtonState
decodeMouseButtons =
    D.field "buttons" D.int
        |> D.andThen mouseButtonFromInt


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
