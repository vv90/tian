module Main exposing (..)

-- import Geo.GeoUtils exposing (..)

import Api.FlightTask exposing (FlightTask)
import Api.NavPoint exposing (Latitude(..), Longitude(..), NavPoint, navPointDecoder)
import AppState
import Browser
import Element exposing (Element, layout, text)
import Element.Font as Font
import Flags exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Http
import List.Extra as ListX
import Map as Map exposing (..)
import MapUtils exposing (..)
import Maybe.Extra as MaybeX
import Nav.Units exposing (Deg(..))
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.Page as Page
import Result.Extra as ResultX
import Utils exposing (..)
import Utils.ApiResult exposing (ApiResult)
import Utils.Deferred exposing (Deferred(..))
import Utils.FlightTaskUtils exposing (taskToMapItems)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- type TaskReadError
--     = NavPointParseError (List DeadEnd)
--     | NavPointMissing String
-- type TrackLoadError
--     = NoFile
--     | NoTask TaskReadError
--       -- | TrackReadError FlightTrackReadError


type FlightTaskPageModel
    = TaskList FlightTaskList.Model
    | TaskForm FlightTaskForm.Model


type alias Model =
    { mapModel : Map.Model

    -- , flightTask : Result TaskReadError FlightTask
    , flightTask : Maybe FlightTask
    , navPoints : Deferred (ApiResult (List NavPoint))

    -- , logViewModel : Result TrackLoadError LogViewDemo.Model
    , pageModel : Page.Model
    , appState : AppState.Model
    }



-- lookupNavPoint : String -> List NavPoint -> Result TaskReadError NavPoint
-- lookupNavPoint name navPoints =
--     ListX.find (\np -> np.name == name) navPoints
--         |> Result.fromMaybe name
--         |> Result.mapError NavPointMissing
-- turnPointsToMapItems : List Turnpoint -> List MapItem
-- turnPointsToMapItems turnpoints =
--   let
--     turnpointToItem tp =
--       case tp of
--         Cylinder r p -> Circle p r
--         Line r p -> Point p
--   in
--     List.map turnpointToItem turnpoints
-- getNavPointsTask : Task (List NavPoint)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- navPointsResult : Result TaskReadError (List NavPoint)
        -- navPointsResult =
        --     navPointLines
        --         |> List.map (Parser.run navPointParser)
        --         |> ResultX.combine
        --         |> Result.mapError NavPointParseError
        -- fromNavPointName : String -> Result TaskReadError NavPoint
        -- fromNavPointName =
        --     \pointName -> navPointsResult |> Result.andThen (lookupNavPoint pointName)
        -- initTaskPoint : ( String, a ) -> Result TaskReadError ( NavPoint, a )
        -- initTaskPoint =
        --     Tuple.mapFirst fromNavPointName >> ResultX.combineFirst
        -- start =
        --     initTaskPoint ( "USMAER", StartLine (Meters 5.0e3) )
        -- finish =
        --     initTaskPoint ( "USMAER", FinishCylinder (Meters 3.0e3) )
        -- turnpoints =
        --     [ ( "U15PANINO", Cylinder (Meters 5.0e2) )
        --     , ( "U43ISRAIL", Cylinder (Meters 5.0e2) )
        --     , ( "U21BITYG", Cylinder (Meters 5.0e2) )
        --     , ( "U44TOKAR", Cylinder (Meters 5.0e2) )
        --     , ( "U14HAVA", Cylinder (Meters 5.0e2) )
        --     , ( "U12DEVI", Cylinder (Meters 5.0e2) )
        --     ]
        --         |> List.map initTaskPoint
        --         |> ResultX.combine
        -- flightTask =
        --     Result.map3 FlightTask start turnpoints finish
        -- flightTask |> Result.map taskToMapItems
        mapModel =
            Map.init
                flags.windowSize
                -- (Result.withDefault [] mapItems)
                9
                -- { lon = LonDeg (Deg 39.662962)
                -- , lat = LatDeg (Deg 52.030558)
                -- }
                -- ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
                ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
    in
    ( { mapModel = mapModel
      , flightTask = Nothing

      --   , logViewModel = Err NoFile
      , navPoints = NotStarted
      , pageModel = Page.init
      , appState =
            AppState.init
                |> AppState.withPendingNavPoints
                |> AppState.withPendingFlightTasks
      }
    , Cmd.batch
        [ Cmd.map AppStateMsg AppState.getNavPointsCmd
        , Cmd.map AppStateMsg AppState.getFlightTasksCmd
        ]
    )


type Msg
    = MapMsg Map.Msg
      -- | LogViewMsg LogViewDemo.Msg
      -- | FlightTaskFormMsg FlightTaskForm.Msg
    | PageMsg Page.Msg
    | AppStateMsg AppState.Msg
      -- | LogRequested
      -- | LogSelected File
      -- | LogLoaded String
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMsg m ->
            let
                ( nextModel, cmd ) =
                    Map.update m model.mapModel
            in
            ( { model | mapModel = nextModel }, Cmd.map MapMsg cmd )

        -- LogViewMsg m ->
        --     model.logViewModel
        --         |> Result.map (LogViewDemo.update m)
        --         |> Result.map
        --             (\( nextModel, cmd ) ->
        --                 ( { model | logViewModel = Ok nextModel }
        --                 , Cmd.map LogViewMsg cmd
        --                 )
        --             )
        --         |> Result.withDefault
        --             ( model, Cmd.none )
        PageMsg m ->
            let
                ( nextModel, cmd ) =
                    Page.update m model.pageModel
            in
            ( { model | pageModel = nextModel }, Cmd.map PageMsg cmd )

        AppStateMsg m ->
            let
                ( nextModel, cmd ) =
                    AppState.update m model.appState
            in
            ( { model | appState = nextModel }, Cmd.map AppStateMsg cmd )

        -- LogRequested ->
        --     ( model
        --     , Select.file [] LogSelected
        --     )
        -- LogSelected file ->
        --     ( model
        --     , Task.perform LogLoaded (File.toString file)
        --     )
        -- LogLoaded content ->
        --     ( { model
        --         | logViewModel =
        --             Result.map2
        --                 LogViewDemo.init
        --                 (Result.mapError NoTask model.flightTask)
        --                 ((parseFlightTrack >> Result.mapError TrackReadError) content)
        -- | flightTrack =
        --     content
        --     |> Parser.run flightTrackParser
        --     |> Result.map (\t -> (t, initPlaybackState t))
        --     |> Result.mapError TrackParsingError
        -- , content = content |> String.lines
        --   }
        -- , Cmd.none
        -- )
        NoMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MapMsg (Map.subscriptions model.mapModel)

        -- , model.logViewModel
        --     |> Result.map (LogViewDemo.subscriptions >> Sub.map LogViewMsg)
        --     |> Result.withDefault Sub.none
        ]


view : Model -> Html Msg
view model =
    let
        -- trackPositionMarker : Maybe Marker
        -- trackPositionMarker =
        --     Result.toMaybe model.logViewModel
        --         |> Maybe.map
        --             (\m ->
        --                 { position = trackPointToGeoPoint m.currentPosition
        --                 , markerType = Glider
        --                 , caption = m.flightTrack.compId ++ " " ++ (getAltitude >> getMeters >> String.fromFloat) m.currentPosition.altitudeGps ++ "m"
        --                 }
        --             )
        -- markers =
        --     MaybeX.unwrap [] List.singleton trackPositionMarker
        mapItems =
            case model.pageModel of
                Page.TaskList pm ->
                    FlightTaskList.selectedFlightTask model.appState.flightTasks pm
                        |> Maybe.map (.entity >> taskToMapItems)
                        |> Maybe.withDefault []

                Page.TaskForm pm ->
                    FlightTaskForm.result pm
                        |> Result.map taskToMapItems
                        |> Result.withDefault []

                Page.TrackUpload _ ->
                    []
    in
    div []
        [ Map.view mapItems model.mapModel |> Html.map MapMsg

        -- , div
        --     [ style "position" "absolute"
        --     , style "top" "10px"
        --     , style "right" "10px"
        --     , style "padding" "10px"
        --     , style "background" "white"
        --     , style "border" "1px solid gray"
        --     , style "border-radius" "10px"
        --     , style "min-width" "200px"
        --     ]
        --     [ viewLog model.logViewModel
        --     ]
        , div
            [ style "position" "absolute"
            , style "top" "10px"
            , style "left" "10px"
            , style "padding" "10px"
            , style "background" "white"
            , style "border" "1px solid gray"
            , style "border-radius" "10px"
            , style "min-width" "200px"
            ]
            [ layout [ Font.size 16 ] <|
                Element.map PageMsg <|
                    Page.view model.appState model.pageModel
            ]
        ]



-- viewLog : Result TrackLoadError LogViewDemo.Model -> Html Msg
-- viewLog rLogModel =
--     case rLogModel of
--         Ok logModel ->
--             LogViewDemo.view logModel |> Html.map LogViewMsg
--         Err NoFile ->
--             div
--                 []
--                 [ p [] [ text "No File" ]
--                 , button [ onClick LogRequested ] [ text "Upload" ]
--                 ]
--         Err (NoTask _) ->
--             text "Failed to read task"
--         Err (TrackReadError e) ->
--             div
--                 []
--                 [ (showFlightTrackReadError >> text) e
--                 , button [ onClick LogRequested ] [ text "Upload" ]
--                 ]
