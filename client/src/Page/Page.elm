module Page.Page exposing (..)

import AppState
import Element exposing (Attribute, Element, column, padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.FlightTrack.FlightTrackUpload as FlightTrackUpload
import Utils.Palette as Palette


type Model
    = TaskList FlightTaskList.Model
    | TaskForm FlightTaskForm.Model
    | TrackUpload FlightTrackUpload.Model


type Page
    = TaskListPage
    | TaskFormPage
    | TrackUploadPage


init : Model
init =
    -- TaskForm <| FlightTaskForm.init ()
    TaskList <| FlightTaskList.init


type Msg
    = TaskFormMsg FlightTaskForm.Msg
    | TaskListMsg FlightTaskList.Msg
    | TrackUploadMsg FlightTrackUpload.Msg
    | PageSelected Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( TaskFormMsg formMsg, TaskForm formModel ) ->
            let
                ( newModel, cmd ) =
                    FlightTaskForm.update formMsg formModel
            in
            ( TaskForm newModel, Cmd.map TaskFormMsg cmd )

        ( TaskFormMsg _, _ ) ->
            ( model, Cmd.none )

        ( TaskListMsg listMsg, TaskList listModel ) ->
            let
                ( newModel, cmd ) =
                    FlightTaskList.update listMsg listModel
            in
            ( TaskList newModel, Cmd.map TaskListMsg cmd )

        ( TaskListMsg _, _ ) ->
            ( model, Cmd.none )

        ( TrackUploadMsg uploadMsg, TrackUpload uploadModel ) ->
            let
                ( newModel, cmd ) =
                    FlightTrackUpload.update uploadMsg uploadModel
            in
            ( TrackUpload newModel, Cmd.map TrackUploadMsg cmd )

        ( TrackUploadMsg _, _ ) ->
            ( model, Cmd.none )

        ( PageSelected TaskListPage, TaskList _ ) ->
            ( model, Cmd.none )

        ( PageSelected TaskListPage, _ ) ->
            ( TaskList FlightTaskList.init, Cmd.none )

        ( PageSelected TaskFormPage, TaskForm _ ) ->
            ( model, Cmd.none )

        ( PageSelected TaskFormPage, _ ) ->
            ( TaskForm FlightTaskForm.init, Cmd.none )

        ( PageSelected TrackUploadPage, TrackUpload _ ) ->
            ( model, Cmd.none )

        ( PageSelected TrackUploadPage, _ ) ->
            ( TrackUpload FlightTrackUpload.init, Cmd.none )


view : AppState.Model -> Model -> Element Msg
view appState model =
    let
        pageBtnAttrs =
            [ padding 5
            , Border.solid
            , Border.rounded 3
            , Border.width 1
            , Border.color <| Palette.darkGray
            ]

        taskListBtnBackground =
            case model of
                TaskList _ ->
                    Background.color Palette.primaryLight

                _ ->
                    Background.color Palette.white

        taskFormBtnBackground =
            case model of
                TaskForm _ ->
                    Background.color Palette.primaryLight

                _ ->
                    Background.color Palette.white

        trackUploadBtnBackground =
            case model of
                TrackUpload _ ->
                    Background.color Palette.primaryLight

                _ ->
                    Background.color Palette.white
    in
    column [ spacing 10 ]
        [ row [ spacing 20 ]
            [ Input.button (taskListBtnBackground :: pageBtnAttrs)
                { onPress = Just <| PageSelected TaskListPage
                , label = text "Task List"
                }
            , Input.button (taskFormBtnBackground :: pageBtnAttrs)
                { onPress = Just <| PageSelected TaskFormPage
                , label = text "Task Form"
                }
            , Input.button (trackUploadBtnBackground :: pageBtnAttrs)
                { onPress = Just <| PageSelected TrackUploadPage
                , label = text "Track Upload"
                }
            ]
        , pageView appState model
        ]


pageView : AppState.Model -> Model -> Element Msg
pageView appState model =
    case model of
        TaskForm formModel ->
            FlightTaskForm.view appState.navPoints formModel
                |> Element.map TaskFormMsg

        TaskList listModel ->
            FlightTaskList.view appState.flightTasks listModel
                |> Element.map TaskListMsg

        TrackUpload uploadModel ->
            FlightTrackUpload.view uploadModel
                |> Element.map TrackUploadMsg
