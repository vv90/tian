module Map exposing (..)
import Html exposing (div, Html, h2, h5, text, button, label, img, object, p, select, option)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (style, src, width, height, class, attribute, value)
import Html.Events.Extra exposing (onChange)
import Canvas exposing (clear, Point, Renderable, shapes, texture, rect, group)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (transform, scale, translate)
import Canvas.Texture as Texture exposing (..)
import Color as Color exposing (..)
import Geo.GeoUtils exposing (..)
import Browser.Events as BE
import Json.Decode as D
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Dict exposing (Dict)
import Flags exposing (WindowSize)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import MapUtils exposing (..)
import Nav.Units exposing (getDeg, getRad, degToRad, Meters(..), Deg(..))
import Geo.Constants exposing (metersPerPixel)
import TimeUtils exposing (..)
import ParsingDemo
import IntersectionDemo
import Nav.FlightTrack exposing (FlightTrackReadError)

type DragState = MovingFrom (Float, Float) | Static 



type DemoModel 
  = ParsingDemoModel ParsingDemo.Model
  | IntersectionDemoModel IntersectionDemo.Model

getDemoPoints : DemoModel -> List GeoPoint 
getDemoPoints demoModel =
  case demoModel of
    ParsingDemoModel _ -> []
    IntersectionDemoModel m -> 
      List.filterMap identity [m.p1, m.p2, m.p3, m.p4, m.intersectionPoint]

type alias Model = 
  { tiles: Dict TileKey (Maybe Texture)
  , tileSources: Dict TileKey String
  , mapItems: List MapItem
  , mapView: MapView
  , dragState: DragState
  , mousePosition: (Float, Float)
  , initPoint: (Float, Float)
  , demoModel: DemoModel
  -- , pointDemoModel: PointDemo.Model
  -- , selectedPoint: Maybe GeoPoint
  }


-- init : (WindowSize, Result MapInitError (List MapItem)) -> Float -> GeoPoint -> Model
-- init (windowSize, mapItems) zoom point =
init : WindowSize -> List MapItem -> Float -> GeoPoint -> Model
init windowSize mapItems zoom point =
  let 
    toTileCoord = ((*) (2^zoom)) >> floor
    (x, y) = toMercatorWeb point |> Tuple.mapBoth toTileCoord toTileCoord
    (centerX, centerY) = (x * tileSize + tileSize//2, y * tileSize + tileSize//2)
    offset = (toFloat centerX - toFloat windowSize.width/2, toFloat centerY - toFloat windowSize.height/2)
    mapView = 
      { height = windowSize.height
      , width = windowSize.width
      , zoom = zoom
      , offset = offset
      }
  in 
    { tiles = Dict.empty
    , tileSources = addTileSources mapView Dict.empty
    , mapItems = mapItems
    , mapView = mapView
    , dragState = Static
    , mousePosition = (0, 0)
    , initPoint = toMercatorWeb point
    , demoModel = ParsingDemoModel (ParsingDemo.init ())
    -- , pointDemoModel = PointDemo.init ()
    -- , selectedPoint = Nothing
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
  | MouseMoved (Float, Float)
  | Resized Int Int
  | ZoomChanged WheelEvent
  | ZoomedIn 
  | ZoomedOut 
  | DragStart (Float, Float)
  | DragMove Bool (Float, Float)
  | DragStop (Float, Float)
  | TileLoaded TileKey (Maybe Texture)
  | DemoChanged String

  | ParsingDemoMsg ParsingDemo.Msg
  | IntersectionDemoMsg IntersectionDemo.Msg
  -- | PointDemoMsg PointDemo.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None -> (model, Cmd.none)
    -- Clicked -> 
    --   ( { model | selectedPoint = model.mousePosition |> Tuple.mapBoth toFloat toFloat |> viewCoordsToGeoPoint model.mapView |> Just}
    --   , Cmd.none
    --   )
    Clicked -> (model, Cmd.none)
    Resized w h -> 
      let
        mapView = model.mapView
      in
        ( model |> setMapView {mapView | width = w, height = h}
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
        mapView = model.mapView
        (offsetX, offsetY) = model.mapView.offset
        point = (offsetX + e.offsetX, offsetY + e.offsetY)
        scaleDelta = negate (e.deltaY) * (1 / 960)
        newZoom = clamp minZoom maxZoom (model.mapView.zoom + scaleDelta)
        scaleCoefficient = 2 ^ scaleDelta
        newOffset = scaleOffset point scaleCoefficient model.mapView.offset
        newMapView = 
          { mapView 
          | offset = newOffset
          , zoom = newZoom
          }
      in
        -- (model, Cmd.none)
        ( setMapView newMapView model
        , Cmd.none
        )
    DragStart xy -> ({ model | dragState = MovingFrom xy }, Cmd.none)
    DragMove isDown (x, y) -> 
      let
        (fromX, fromY) = 
          case model.dragState of
            Static -> (x, y)
            MovingFrom xy -> xy
        (offsetX, offsetY) = model.mapView.offset
        newOffset = (offsetX - (x - fromX), offsetY - (y - fromY))
        mapView = model.mapView
        newMapView = { mapView | offset = newOffset }
      in
        ( { model  | dragState = if isDown then MovingFrom (x, y) else Static }
          |> setMapView newMapView
        , Cmd.none
        )
    DragStop _ -> ({ model | dragState = Static }, Cmd.none)
    TileLoaded tileKey t -> 
      ( { model | tiles = Dict.update tileKey (\_ -> Just t) model.tiles
        }
      , Cmd.none
      )
    
    DemoChanged str ->
      case str of
        "Parsing" -> 
          ({ model | demoModel = ParsingDemo.init () |> ParsingDemoModel }, Cmd.none)
        "Intersection" ->
          ({ model | demoModel = IntersectionDemo.init () |> IntersectionDemoModel }, Cmd.none)
        _ ->
          (model, Cmd.none)


    ParsingDemoMsg parsingDemoMsg -> 
      case model.demoModel of
        ParsingDemoModel parsingModel -> 
          let 
            (m, c) = ParsingDemo.update parsingDemoMsg parsingModel
          in
            ( { model | demoModel = ParsingDemoModel m }
            , Cmd.map ParsingDemoMsg c 
            )
        _ -> ( model, Cmd.none )

    IntersectionDemoMsg intersectionDemoMsg ->
      case model.demoModel of
        IntersectionDemoModel intersectionModel ->
          let
            (m, c) = IntersectionDemo.update intersectionDemoMsg intersectionModel
            
          in
            ( { model | demoModel = IntersectionDemoModel m }
            , Cmd.map IntersectionDemoMsg c 
            )
        _ -> (model, Cmd.none)
  
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


view : Model -> (List Marker) -> Html Msg
view model markers = 
  let
    -- scaleCoefficient = getScaleCoefficient model.mapView
    scaleCoefficient = 2 ^ (model.mapView.zoom - (floor >> toFloat) model.mapView.zoom) 
    (offsetX, offsetY) = scaleCoords scaleCoefficient model.mapView.offset
    (cursorLocalX, cursorLocalY) = model.mousePosition
    cursorGlobal = Tuple.mapBoth ((+) cursorLocalX) ((+) cursorLocalY) model.mapView.offset
    -- cursorGeoPoint = model.mousePosition |> Tuple.mapBoth toFloat toFloat |> viewCoordsToGeoPoint model.mapView
    clearAll = clear ( 0, 0 ) (toFloat model.mapView.width) (toFloat model.mapView.height)

    lookupTileTexture tileKey =  
      case Dict.get (normalizeTileKey tileKey) model.tiles of
        Just txtr -> (tileKey, txtr)
        Nothing -> (tileKey, Nothing)
      
    renderedTiles = 
      tilesInView model.mapView
      |> List.map (lookupTileTexture >> renderTile)
      
    renderedMarkers = List.map (renderMarker model.mapView) markers
    -- renderedMarkersSvg = List.map (renderMarkerSvg model.mapView) model.markers
    renderedDemoPoints = 
      getDemoPoints model.demoModel
      |> List.map (renderPoint model.mapView)
    renderedMapItems = 
      -- ResultX.unpack 
        -- (always []) 
        (List.map (renderVectorItem model.mapView)) 
        model.mapItems

    -- withOptPoint : Maybe GeoPoint -> List (Svg Msg) -> List (Svg Msg)
    -- withOptPoint optPoint mapItems =
    --   optPoint 
    --   |> Maybe.map (\p -> renderPoint model.mapView p :: mapItems)
    --   |> Maybe.withDefault mapItems
  in
    div 
      []
      [
        div 
          [ onClick (IntersectionDemoMsg (((viewCoordsToGeoPoint model.mapView) >> IntersectionDemo.PointSelected) model.mousePosition))
          , on "mousedown" (D.map DragStart decodePosition)
          , on "wheel" (D.map ZoomChanged decodeWheelEvent)
          ]
          ([ Canvas.toHtmlWith 
            { width = model.mapView.width
            , height = model.mapView.height
            , textures = 
                Dict.toList model.tileSources 
                |> List.map (\(key, url) -> Texture.loadFromImageUrl url (TileLoaded key)) 
                  
            }
            [ ]
            [ group 
                [ transform 
                  [ scale scaleCoefficient scaleCoefficient
                  , translate (negate offsetX ) (negate offsetY )
                  ] 
                ] 
                (clearAll :: renderedTiles)
            ]  
          , Svg.svg 
            [ SvgAttr.width (String.fromInt model.mapView.width)
            , SvgAttr.height (String.fromInt model.mapView.height)
            , SvgAttr.viewBox (String.join " " ["0", "0", (String.fromInt model.mapView.width), (String.fromInt model.mapView.height)])
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0" 
            ]
            -- ( model.selectedPoint 
            --   |> Maybe.map (\p -> renderPoint model.mapView p :: renderedMapItems) 
            --   |> Maybe.withDefault renderedMapItems
            -- )
            ( renderedMapItems ++ renderedMarkers ++ renderedDemoPoints
            -- |> withOptPoint model.selectedPoint 
            -- |> withOptPoint model.pointDemoModel.projectedPoint
            )
          
          ] )--++ renderedMarkers)
        ,
        div 
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
                      (\x -> x/(toFloat tileSize * 2^model.mapView.zoom))
                      (\y -> y/(toFloat tileSize * 2^model.mapView.zoom))
                  |> (fromMercatorWeb >> (\p -> ((getLon >> getDeg) p.lon, (getLat >> getDeg) p.lat)))
                  |> Tuple.mapBoth String.fromFloat String.fromFloat 
                  |> stringFromTuple
                ]
              )]
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
                ])]
          ]
          -- demo component window
          , div 
            [ style "position" "absolute"
            , style "top" "10px"
            , style "left" "10px"
            , style "padding" "10px" 
            , style "background" "white"
            , style "border" "1px solid gray"
            , style "border-radius" "10px"
            ] 
            
            [ select [ onChange DemoChanged ] 
              [ option [ value "Parser" ] [ text "Parser" ]
              , option [ value "Intersection" ] [ text "Intersection" ]
              ]
            , viewDemo model.demoModel
            ]
      ]

viewDemo : DemoModel -> Html Msg
viewDemo demoModel =
  case demoModel of
    ParsingDemoModel parsingModel -> 
      Html.map ParsingDemoMsg (ParsingDemo.view parsingModel)
    IntersectionDemoModel intersectionModel ->
      Html.map IntersectionDemoMsg (IntersectionDemo.view intersectionModel)
  
    

renderTile : (TileKey, Maybe Texture) -> Renderable
renderTile ((x, y, _), t) = 
  let
    point : Point
    point = (toFloat (x * tileSize), toFloat (y * tileSize))
  in
  
    case t of
      Nothing -> shapes [ fill Color.white ] [ rect point (toFloat tileSize) (toFloat tileSize) ]
      Just txtr -> texture [] point txtr

renderVectorItem : MapView -> MapItem -> Svg.Svg Msg
renderVectorItem mapView item =
  case item of
    Point point -> renderPoint mapView point
    Circle point radius -> renderCircle mapView point radius
    Line points -> renderLine mapView points
    Polygon points -> renderPolygon mapView points

renderPoint : MapView -> GeoPoint -> Svg.Svg Msg
renderPoint mapView point =
  let     
    (x, y) = geoPointToViewCoords mapView point
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

renderCircle : MapView -> GeoPoint -> Meters -> Svg Msg
renderCircle mapView point (Meters radius) =
  let     
    (x, y) = geoPointToViewCoords mapView point
    scaleCoefficient = scaleFromZoom mapView.zoom
    rPixels = 
      metersPerPixel (floor mapView.zoom)
      |> Maybe.map (\(Meters m) -> radius/(cos (point.lat |> getLat |> degToRad |> getRad) * m/scaleCoefficient))
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
        (geoPointToViewCoords mapView >> (\(x, y) -> String.join "," [String.fromFloat x, String.fromFloat y])) 
        points
  in
    Svg.polyline
    [ SvgAttr.points (String.join " " xys)
    , SvgAttr.strokeWidth "2"
    , SvgAttr.stroke "black" ]
    []

renderPolygon : MapView -> List GeoPoint -> Svg Msg
renderPolygon mapView points =
  let
    xys = 
      List.map 
        (geoPointToViewCoords mapView >> (\(x, y) -> String.join "," [String.fromFloat x, String.fromFloat y])) 
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
    (x, y) = geoPointToViewCoords mapView marker.position 
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
    w = 40
    h = 40
    -- markerSrc = 
    --   case marker.markerType of
    --     Glider -> "assets/glider.svg" 
    
    (x, y) = geoPointToViewCoords mapView marker.position
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
      [Svg.text marker.caption]
    ]

{-
The "buttons" value is 1 when "left-click" is pressed, so we use that to
detect zombie drags.
-}
decodeButtons : D.Decoder Bool
decodeButtons =
  D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)

decodePosition : D.Decoder (Float, Float)
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