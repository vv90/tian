module Main exposing (..)

-- import Geo.GeoUtils exposing (..)
-- import Utils exposing (..)

import Api.Entity exposing (Entity)
import Api.FlightTask exposing (FlightTask)
import Api.Geo exposing (Latitude(..), Longitude(..))
import Api.NavPoint exposing (NavPoint, navPointDecoder)
import AppState
import Browser
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredToMaybe)
import Common.Effect as Effect
import Common.FlightTaskUtils exposing (navPoints, taskToMapItems)
import Common.TaskProgressUtils exposing (progressPointsToMapItems)
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
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.FlightTaskPage as FlightTaskPage
import Page.FlightTrack.FlightTrackUpload as FlightTrackUpload
import Page.Page as Page
import Result.Extra as ResultX


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { mapModel : Map.Model
    , flightTaskPage : FlightTaskPage.Model
    , flightTrackPage : Maybe FlightTrackUpload.Model
    , appState : AppState.Model
    }


withAppState : AppState.Model -> Model -> Model
withAppState appState model =
    { model | appState = appState }


withFlightTaskPage : FlightTaskPage.Model -> Model -> Model
withFlightTaskPage ftpModel model =
    { model | flightTaskPage = ftpModel }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        mapModel =
            Map.init
                flags.windowSize
                9
                ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
    in
    ( { mapModel = mapModel
      , flightTaskPage = FlightTaskPage.init
      , flightTrackPage = Nothing
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
    | FlightTaskPageMsg FlightTaskPage.Msg
    | FlightTrackPageMsg FlightTrackUpload.Msg
    | AppStateMsg AppState.Msg
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

        FlightTaskPageMsg ftPageMsg ->
            let
                ( nextModel, cmd, effs ) =
                    FlightTaskPage.update ftPageMsg model.flightTaskPage

                applyEffect e =
                    case e of
                        FlightTaskPage.FlightTaskSaved _ ->
                            ( \m -> m |> withAppState (m.appState |> AppState.withPendingFlightTasks)
                            , Cmd.map AppStateMsg AppState.getFlightTasksCmd
                            )
            in
            ( model |> withFlightTaskPage nextModel, Cmd.map FlightTaskPageMsg cmd )
                |> Effect.applyAll applyEffect effs

        FlightTrackPageMsg ftPageMsg ->
            case model.flightTrackPage of
                Just ftpModel ->
                    let
                        ( nextModel, cmd ) =
                            FlightTrackUpload.update ftPageMsg ftpModel
                    in
                    ( { model | flightTrackPage = Just nextModel }
                    , Cmd.map FlightTrackPageMsg cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        AppStateMsg m ->
            let
                ( nextModel, cmd ) =
                    AppState.update m model.appState
            in
            ( { model | appState = nextModel }, Cmd.map AppStateMsg cmd )

        NoMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MapMsg (Map.subscriptions model.mapModel)
        , Sub.map FlightTaskPageMsg (FlightTaskPage.subscriptions model.flightTaskPage)
        ]


type DetachedPosition
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


detachedView : DetachedPosition -> Element msg -> Html msg
detachedView pos content =
    let
        viewStyle =
            [ style "position" "absolute"
            , style "margin" "10px"
            , style "padding" "10px"
            , style "background" "white"
            , style "border" "1px solid gray"
            , style "border-radius" "10px"
            ]

        positionStyle =
            case pos of
                TopLeft ->
                    [ style "top" "0", style "left" "0" ]

                TopRight ->
                    [ style "top" "0", style "right" "0" ]

                BottomLeft ->
                    [ style "bottom" "0", style "left" "0" ]

                BottomRight ->
                    [ style "bottom" "0", style "right" "0" ]
    in
    div
        (viewStyle ++ positionStyle)
        [ Element.layout
            [ Font.size 16 ]
            content
        ]


selectedFlightTask : Deferred (ApiResult (List (Entity Int FlightTask))) -> Int -> Maybe (Entity Int FlightTask)
selectedFlightTask flightTasksD taskId =
    case flightTasksD of
        Resolved (Ok items) ->
            ListX.find (\{ key } -> key == taskId) items

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    let
        mapItems =
            case model.flightTaskPage.page of
                FlightTaskPage.AddTask pm ->
                    FlightTaskForm.result pm
                        |> Result.map taskToMapItems
                        |> Result.withDefault []

                FlightTaskPage.UploadTrack pm ->
                    let
                        task =
                            Maybe.map .entity <| selectedFlightTask model.appState.flightTasks pm.taskId

                        taskItems =
                            task
                                |> Maybe.map taskToMapItems
                                |> Maybe.withDefault []

                        -- trackItems =
                        --     (deferredToMaybe >> Maybe.andThen Result.toMaybe) pm.taskProgress
                        --         |> Maybe.map (List.concatMap (.points >> progressPointsToMapItems))
                        --         |> Maybe.withDefault []
                        compId =
                            (deferredToMaybe >> Maybe.andThen Result.toMaybe >> Maybe.andThen List.head >> Maybe.map .compId) pm.taskProgress

                        point =
                            pm.playerModel |> Maybe.andThen .currentPoint

                        findTargetNavPoint : FlightTask -> String -> Maybe NavPoint
                        findTargetNavPoint t name =
                            navPoints t |> ListX.find (\np -> np.name == name)

                        target =
                            point
                                |> Maybe.andThen .target
                                |> Maybe.map Tuple.first
                                |> MaybeX.andThen2 findTargetNavPoint task

                        pointItems =
                            Maybe.map2
                                (\cid p -> [ Marker ( p.lat, p.lon ) cid ])
                                compId
                                point
                                |> Maybe.withDefault []
                    in
                    taskItems ++ pointItems

                FlightTaskPage.SelectTask ->
                    []

        selectedTask =
            case model.flightTaskPage.page of
                -- FlightTaskPage.SelectTask pm ->
                --     pm.selectedFlightTaskId
                --         |> Maybe.andThen (selectedFlightTask model.appState.flightTasks)
                FlightTaskPage.UploadTrack pm ->
                    selectedFlightTask model.appState.flightTasks pm.taskId

                _ ->
                    Nothing
    in
    div []
        [ Map.view mapItems model.mapModel |> Html.map MapMsg
        , detachedView TopLeft <|
            Element.map FlightTaskPageMsg <|
                FlightTaskPage.view
                    { navPoints = model.appState.navPoints
                    , flightTasks = model.appState.flightTasks
                    }
                    model.flightTaskPage

        -- , MaybeX.unwrap
        --     (div [] [])
        --     (\(taskId, ftpModel) ->
        --         detachedView TopRight <|
        --             Element.map FlightTrackPageMsg <|
        --                 FlightTrackUpload.view ftpModel
        --     )
        --     model.flightTrackPage
        -- , detachedView TopRight <|
        --     Element.text "Hello"
        ]
