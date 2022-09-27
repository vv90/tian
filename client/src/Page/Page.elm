module Page.Page exposing (..)

import AppState
import Element exposing (Element)
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList


type Model
    = TaskList FlightTaskList.Model
    | TaskForm FlightTaskForm.Model


init : Model
init =
    TaskForm <| FlightTaskForm.init ()


type Msg
    = TaskFormMsg FlightTaskForm.Msg


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


view : AppState.Model -> Model -> Element Msg
view appState model =
    case model of
        TaskForm formModel ->
            FlightTaskForm.view appState.navPoints formModel
                |> Element.map TaskFormMsg

        TaskList listModel ->
            FlightTaskList.view listModel
                |> Element.map TaskFormMsg
