module Map exposing (..)

-- import Geo.Constants exposing (metersPerPixel)
-- import Geo.GeoUtils exposing (..)
-- import Nav.Units exposing (Deg(..), Meters(..), degToRad, getDeg, getRad)

import Api.Geo exposing (Distance(..), Latitude(..), Longitude(..))
import Browser.Events as BE
import Canvas exposing (Point, Renderable, clear, group, rect, shapes, texture)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Canvas.Texture as Texture exposing (..)
import Color as Color exposing (..)
import Dict exposing (Dict)
import Flags exposing (WindowSize)
import Html exposing (Html, button, div, h2, h5, img, label, object, option, p, select, text)
import Html.Attributes exposing (attribute, class, height, src, style, value, width)
import Html.Events exposing (on, onClick)
import Json.Decode as D
import List.Extra as ListX
import MapUtils exposing (..)
import Maybe.Extra as MaybeX
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import TimeUtils exposing (..)


type DragState
    = MovingFrom ( Float, Float )
    | Static


type alias Model =
    { tiles : Dict TileKey (Maybe Texture)
    , tileSources : Dict TileKey String
    , mapView : MapView
    , dragState : DragState
    , mousePosition : ( Float, Float )
    }


init : WindowSize -> Float -> GeoPoint -> Model
init windowSize zoom point =
    let
        toTileCoord =
            (*) (2 ^ zoom) >> floor

        ( x, y ) =
            toMercatorWeb point |> Tuple.mapBoth toTileCoord toTileCoord

        ( centerX, centerY ) =
            ( x * tileSize + tileSize // 2, y * tileSize + tileSize // 2 )

        offset =
            ( toFloat centerX - toFloat windowSize.width / 2, toFloat centerY - toFloat windowSize.height / 2 )

        mapView =
            { height = windowSize.height
            , width = windowSize.width
            , zoom = zoom
            , offset = offset
            }
    in
    { tiles = Dict.empty
    , tileSources = addTileSources mapView Dict.empty
    , mapView = mapView
    , dragState = Static
    , mousePosition = ( 0, 0 )
    }


setMapView : MapView -> Model -> Model
setMapView mapView model =
    { model
        | mapView = mapView
        , tileSources = addTileSources mapView model.tileSources
    }


type Msg
    = None
    | Clicked
    | MouseMoved ( Float, Float )
    | Resized Int Int
    | ZoomChanged WheelEvent
    | ZoomedIn
    | ZoomedOut
    | DragStart ( Float, Float )
    | DragMove Bool ( Float, Float )
    | DragStop ( Float, Float )
    | TileLoaded TileKey (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        -- Clicked ->
        --   ( { model | selectedPoint = model.mousePosition |> Tuple.mapBoth toFloat toFloat |> viewCoordsToGeoPoint model.mapView |> Just}
        --   , Cmd.none
        --   )
        Clicked ->
            ( model, Cmd.none )

        Resized w h ->
            let
                mapView =
                    model.mapView
            in
            ( model |> setMapView { mapView | width = w, height = h }
            , Cmd.none
            )

        MouseMoved xy ->
            ( { model | mousePosition = xy }
            , Cmd.none
            )

        ZoomedIn ->
            ( setMapView (changeZoom 2 model.mapView) model
            , Cmd.none
            )

        ZoomedOut ->
            ( setMapView (changeZoom 0.5 model.mapView) model
            , Cmd.none
            )

        ZoomChanged e ->
            let
                mapView =
                    model.mapView

                ( offsetX, offsetY ) =
                    model.mapView.offset

                point =
                    ( offsetX + e.offsetX, offsetY + e.offsetY )

                scaleDelta =
                    negate e.deltaY * (1 / 960)

                newZoom =
                    clamp minZoom maxZoom (model.mapView.zoom + scaleDelta)

                scaleCoefficient =
                    2 ^ scaleDelta

                newOffset =
                    scaleOffset point scaleCoefficient model.mapView.offset

                newMapView =
                    { mapView
                        | offset = newOffset
                        , zoom = newZoom
                    }
            in
            ( setMapView newMapView model
            , Cmd.none
            )

        DragStart xy ->
            ( { model | dragState = MovingFrom xy }, Cmd.none )

        DragMove isDown ( x, y ) ->
            let
                ( fromX, fromY ) =
                    case model.dragState of
                        Static ->
                            ( x, y )

                        MovingFrom xy ->
                            xy

                ( offsetX, offsetY ) =
                    model.mapView.offset

                newOffset =
                    ( offsetX - (x - fromX), offsetY - (y - fromY) )

                mapView =
                    model.mapView

                newMapView =
                    { mapView | offset = newOffset }
            in
            ( { model
                | dragState =
                    if isDown then
                        MovingFrom ( x, y )

                    else
                        Static
              }
                |> setMapView newMapView
            , Cmd.none
            )

        DragStop _ ->
            ( { model | dragState = Static }, Cmd.none )

        TileLoaded tileKey t ->
            ( { model
                | tiles = Dict.update tileKey (\_ -> Just t) model.tiles
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubs =
            case model.dragState of
                Static ->
                    Sub.none

                MovingFrom _ ->
                    Sub.batch
                        [ BE.onMouseMove (D.map2 DragMove decodeButtons decodePosition)
                        , BE.onMouseUp (D.map DragStop decodePosition)
                        ]
    in
    Sub.batch
        [ dragSubs
        , BE.onMouseMove (D.map MouseMoved decodePosition)
        , BE.onResize Resized

        -- , BE.onMouseDown (D.map DragStart decodePosition)
        ]


view : List MapItem -> Model -> Html Msg
view mapItems model =
    let
        -- scaleCoefficient = getScaleCoefficient model.mapView
        scaleCoefficient =
            2 ^ (model.mapView.zoom - (floor >> toFloat) model.mapView.zoom)

        ( offsetX, offsetY ) =
            scaleCoords scaleCoefficient model.mapView.offset

        ( cursorLocalX, cursorLocalY ) =
            model.mousePosition

        cursorGlobal =
            Tuple.mapBoth ((+) cursorLocalX) ((+) cursorLocalY) model.mapView.offset

        -- cursorGeoPoint = model.mousePosition |> Tuple.mapBoth toFloat toFloat |> viewCoordsToGeoPoint model.mapView
        clearAll =
            clear ( 0, 0 ) (toFloat model.mapView.width) (toFloat model.mapView.height)

        lookupTileTexture tileKey =
            case Dict.get (normalizeTileKey tileKey) model.tiles of
                Just txtr ->
                    ( tileKey, txtr )

                Nothing ->
                    ( tileKey, Nothing )

        renderedTiles =
            tilesInView model.mapView
                |> List.map (lookupTileTexture >> renderTile)

        -- renderedMarkers =
        --     List.map (renderMarker model.mapView) markers
        -- renderedMarkersSvg = List.map (renderMarkerSvg model.mapView) model.markers
        -- renderedDemoPoints =
        --     getDemoPoints model.demoModel
        --         |> List.map (renderPoint model.mapView)
        renderedMapItems =
            -- ResultX.unpack
            -- (always [])
            List.map (renderVectorItem model.mapView)
                mapItems

        showMapItem : MapItem -> String
        showMapItem item =
            case item of
                Point ( LatitudeDegrees lat, LongitudeDegrees lon ) ->
                    "Point " ++ (lat |> String.fromFloat) ++ " " ++ (lon |> String.fromFloat)

                Line ps ->
                    "Line " ++ (ps |> List.map (\( LatitudeDegrees lat, LongitudeDegrees lon ) -> (lat |> String.fromFloat) ++ " " ++ (lon |> String.fromFloat)) |> String.join ", ")

                Circle ( LatitudeDegrees lat, LongitudeDegrees lon ) (DistanceMeters r) ->
                    "Circle " ++ (lat |> String.fromFloat) ++ " " ++ (lon |> String.fromFloat) ++ " " ++ (r |> String.fromFloat)

        -- withOptPoint : Maybe GeoPoint -> List (Svg Msg) -> List (Svg Msg)
        -- withOptPoint optPoint mapItems =
        --   optPoint
        --   |> Maybe.map (\p -> renderPoint model.mapView p :: mapItems)
        --   |> Maybe.withDefault mapItems
    in
    div
        []
        [ div
            [ onClick Clicked
            , on "mousedown" (D.map DragStart decodePosition)
            , on "wheel" (D.map ZoomChanged decodeWheelEvent)
            ]
            [ Canvas.toHtmlWith
                { width = model.mapView.width
                , height = model.mapView.height
                , textures =
                    Dict.toList model.tileSources
                        |> List.map (\( key, url ) -> Texture.loadFromImageUrl url (TileLoaded key))
                }
                []
                [ group
                    [ transform
                        [ scale scaleCoefficient scaleCoefficient
                        , translate (negate offsetX) (negate offsetY)
                        ]
                    ]
                    (clearAll :: renderedTiles)
                ]
            , Svg.svg
                [ SvgAttr.width (String.fromInt model.mapView.width)
                , SvgAttr.height (String.fromInt model.mapView.height)
                , SvgAttr.viewBox (String.join " " [ "0", "0", String.fromInt model.mapView.width, String.fromInt model.mapView.height ])
                , style "position" "absolute"
                , style "left" "0"
                , style "top" "0"
                ]
                -- ( model.selectedPoint
                --   |> Maybe.map (\p -> renderPoint model.mapView p :: renderedMapItems)
                --   |> Maybe.withDefault renderedMapItems
                -- )
                (renderedMapItems
                 -- |> withOptPoint model.selectedPoint
                 -- |> withOptPoint model.pointDemoModel.projectedPoint
                )
            ]

        --++ renderedMarkers)
        , div
            [ style "position" "absolute"
            , style "bottom" "10px"
            , style "left" "10px"
            , style "padding" "10px"
            , style "background" "white"
            , style "border" "1px solid gray"
            , style "border-radius" "10px"
            ]
            -- map controls
            [ div []
                [ button [ onClick ZoomedOut ] [ text "Smallify" ]
                , button [ onClick ZoomedIn ] [ text "Enbiggen" ]
                ]
            , h5 []
                -- mouse position debug info
                [ text
                    (String.join
                        ", "
                        [ Tuple.first model.mousePosition |> String.fromFloat
                        , Tuple.second model.mousePosition |> String.fromFloat
                        , scaleCoords scaleCoefficient model.mousePosition
                            |> Tuple.mapBoth String.fromFloat String.fromFloat
                            |> stringFromTuple
                        , cursorGlobal
                            |> Tuple.mapBoth
                                (\x -> x / (toFloat tileSize * 2 ^ model.mapView.zoom))
                                (\y -> y / (toFloat tileSize * 2 ^ model.mapView.zoom))
                            -- |> (fromMercatorWeb >> (\p -> ( (getLon >> getDeg) p.lon, (getLat >> getDeg) p.lat )))
                            |> fromMercatorWeb
                            |> (\( LatitudeDegrees lat, LongitudeDegrees lon ) -> ( lat, lon ))
                            |> Tuple.mapBoth String.fromFloat String.fromFloat
                            |> stringFromTuple
                        ]
                    )
                ]
            , h5 []
                -- map view offset debug info
                [ text
                    (String.join
                        ", "
                        [ Tuple.first model.mapView.offset |> String.fromFloat
                        , Tuple.second model.mapView.offset |> String.fromFloat
                        , String.fromFloat model.mapView.zoom
                        ]
                    )
                ]
            , h5 []
                -- map tiles debug info
                [ text
                    (String.join
                        ", "
                        [ "Total: " ++ (Dict.size model.tileSources |> String.fromInt)
                        , "Loaded: " ++ (Dict.size model.tiles |> String.fromInt)
                        , "Succeded: " ++ (Dict.values model.tiles |> ListX.count MaybeX.isJust |> String.fromInt)
                        , "Rendered: " ++ (List.length renderedTiles |> String.fromInt)
                        , "Scale coefficient: " ++ String.fromFloat (scaleFromZoom model.mapView.zoom)
                        ]
                    )
                ]

            -- , h5 []
            --     (List.map (showMapItem >> text) mapItems)
            ]
        ]


renderTile : ( TileKey, Maybe Texture ) -> Renderable
renderTile ( ( x, y, _ ), t ) =
    let
        point : Point
        point =
            ( toFloat (x * tileSize), toFloat (y * tileSize) )
    in
    case t of
        Nothing ->
            shapes [ fill Color.white ] [ rect point (toFloat tileSize) (toFloat tileSize) ]

        Just txtr ->
            texture [] point txtr


renderVectorItem : MapView -> MapItem -> Svg.Svg Msg
renderVectorItem mapView item =
    case item of
        Point point ->
            renderPoint mapView point

        Circle point radius ->
            renderCircle mapView point radius

        Line points ->
            renderLine mapView points



-- Polygon points ->
--     renderPolygon mapView points


renderPoint : MapView -> GeoPoint -> Svg.Svg Msg
renderPoint mapView point =
    let
        ( x, y ) =
            geoPointToViewCoords mapView point
    in
    Svg.g
        []
        [ Svg.circle
            [ SvgAttr.cx (String.fromFloat x)
            , SvgAttr.cy (String.fromFloat y)
            , SvgAttr.r "5"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.stroke "black"
            , SvgAttr.fill "transparent"
            ]
            []

        -- , Svg.text_
        --   [ SvgAttr.x (String.fromFloat x)
        --   , SvgAttr.y (String.fromFloat y)
        --   , SvgAttr.dy "10"
        --   , SvgAttr.textAnchor "middle"
        --   ]
        --   [Svg.text "USMAER"]
        ]


renderCircle : MapView -> GeoPoint -> Distance -> Svg Msg
renderCircle mapView point (DistanceMeters radius) =
    let
        ( x, y ) =
            geoPointToViewCoords mapView point

        ( LatitudeDegrees lat, LongitudeDegrees lon ) =
            point

        scaleCoefficient =
            scaleFromZoom mapView.zoom

        rPixels =
            metersPerPixel (floor mapView.zoom)
                |> Maybe.map (\(DistanceMeters m) -> radius / (cos (degrees lat) * m / scaleCoefficient))
                |> MaybeX.unpack (always 5) identity
    in
    Svg.circle
        [ SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r (String.fromFloat rPixels)
        , SvgAttr.strokeWidth "2"
        , SvgAttr.stroke "black"
        , SvgAttr.fill "transparent"
        ]
        []


renderLine : MapView -> List GeoPoint -> Svg Msg
renderLine mapView points =
    let
        xys =
            List.map
                (geoPointToViewCoords mapView >> (\( x, y ) -> String.join "," [ String.fromFloat x, String.fromFloat y ]))
                points
    in
    Svg.polyline
        [ SvgAttr.points (String.join " " xys)
        , SvgAttr.strokeWidth "2"
        , SvgAttr.stroke "black"
        ]
        []


renderPolygon : MapView -> List GeoPoint -> Svg Msg
renderPolygon mapView points =
    let
        xys =
            List.map
                (geoPointToViewCoords mapView >> (\( x, y ) -> String.join "," [ String.fromFloat x, String.fromFloat y ]))
                points
    in
    Svg.polygon
        [ SvgAttr.points (String.join " " xys)
        , SvgAttr.strokeWidth "2"
        , SvgAttr.stroke "black"
        ]
        []


renderMarkerSvg : MapView -> Marker -> Svg Msg
renderMarkerSvg mapView marker =
    let
        ( x, y ) =
            geoPointToViewCoords mapView marker.position
    in
    case marker.markerType of
        Glider ->
            Svg.svg
                [ width 40
                , height 40
                , style "position" "absolute"
                , style "top" (String.fromFloat (y - 20) ++ "px")
                , style "left" (String.fromFloat (x - 20) ++ "px")
                , attribute "data-src" "assets/glider.svg"
                ]
                []


renderMarker : MapView -> Marker -> Svg Msg
renderMarker mapView marker =
    let
        w =
            40

        h =
            40

        -- markerSrc =
        --   case marker.markerType of
        --     Glider -> "assets/glider.svg"
        ( x, y ) =
            geoPointToViewCoords mapView marker.position
    in
    Svg.g
        []
        [ Svg.circle
            [ SvgAttr.cx (String.fromFloat x)
            , SvgAttr.cy (String.fromFloat y)
            , SvgAttr.r "3"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.stroke "black"
            , SvgAttr.fill "transparent"
            ]
            []
        , Svg.text_
            [ SvgAttr.x (String.fromFloat x)
            , SvgAttr.y (String.fromFloat y)
            , SvgAttr.dy "-10"
            , SvgAttr.textAnchor "middle"
            ]
            [ Svg.text marker.caption ]
        ]



{-
   The "buttons" value is 1 when "left-click" is pressed, so we use that to
   detect zombie drags.
-}


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
