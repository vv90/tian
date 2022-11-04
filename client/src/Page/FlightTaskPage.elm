module Page.FlightTaskPage exposing (..)

import Api.Entity exposing (Entity, entityDecoder)
import Api.FlightTask exposing (FlightTask, flightTaskDecoder)
import Api.NavPoint exposing (NavPoint)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)
import Common.Effect as Effect exposing (EffectSet, effect)
import Element exposing (Element, text)
import Http
import Json.Decode as D
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.FlightTrack.FlightTrackUpload as FlightTrackUpload


type Page
    = SelectTask
    | AddTask FlightTaskForm.Model
    | UploadTrack FlightTrackUpload.Model


type alias Model =
    { page : Page

    -- , flightTasks : Deferred (ApiResult (List (Entity Int FlightTask)))
    }


init : Model
init =
    { page = SelectTask

    -- , flightTasks = Deferred.init
    }



-- hasError : Model -> Maybe String
-- hasError model =
--     model.error
-- withFlightTasksPending : Model -> Model
-- withFlightTasksPending model =
--     { model | flightTasks = setPending model.flightTasks }
-- withFlightTasks : ApiResult (List (Entity Int FlightTask)) -> Model -> Model
-- withFlightTasks result model =
--     case ( result, model.flightTasks ) of
--         ( Ok fts, _ ) ->
--             { model | flightTasks = Resolved <| Ok fts }
--         ( Err err, NotStarted ) ->
--             { model | flightTasks = Resolved <| Err err }
--         ( Err err, InProgress ) ->
--             { model | flightTasks = Resolved <| Err err }
--         ( Err _, Updating _ ) ->
--             model
--         ( Err _, Resolved _ ) ->
--             model


withPage : Page -> Model -> Model
withPage page model =
    { model | page = page }



-- getFlightTasksCmd : Cmd Msg
-- getFlightTasksCmd =
--     Http.get
--         { url = "http://0.0.0.0:8081/task"
--         , expect =
--             Http.expectJson
--                 (Finished >> GetFlightTasks)
--                 (D.list <| entityDecoder D.int flightTaskDecoder)
--         }


type Msg
    = FlightTaskSelected Int --(Entity Int FlightTask)
      -- | FlightTaskCreated (Entity Int FlightTask)
    | NavBackTriggered
    | CreateTaskTriggered
      -- | FlightTaskListMsg FlightTaskList.Msg
    | FlightTaskFormMsg FlightTaskForm.Msg
    | FlightTrackUploadMsg FlightTrackUpload.Msg



-- | GetFlightTasks (AsyncOperationStatus (ApiResult (List (Entity Int FlightTask))))


type Effect
    = FlightTaskSaved Int


update : Msg -> Model -> ( Model, Cmd Msg, EffectSet Effect )
update msg model =
    case ( msg, model.page ) of
        ( FlightTaskSelected taskId, _ ) ->
            ( model |> withPage (UploadTrack (FlightTrackUpload.init taskId))
            , Cmd.none
            , Effect.none
            )

        -- ( FlightTaskCreated _, _ ) ->
        --     ( { model | page = SelectTask FlightTaskList.init }
        --     , Cmd.none
        --     )
        ( NavBackTriggered, _ ) ->
            ( model |> withPage SelectTask
            , Cmd.none
            , Effect.none
            )

        ( CreateTaskTriggered, _ ) ->
            ( model |> withPage (AddTask FlightTaskForm.init)
            , Cmd.none
            , Effect.none
            )

        -- ( FlightTaskListMsg flightTaskListMsg, SelectTask flightTaskList ) ->
        --     let
        --         ( newFlightTaskList, flightTaskListCmd, _ ) =
        --             FlightTaskList.update flightTaskListMsg flightTaskList
        --     in
        --     ( model |> withPage (SelectTask newFlightTaskList)
        --     , Cmd.map FlightTaskListMsg flightTaskListCmd
        --     , Effect.none
        --     )
        -- ( FlightTaskListMsg _, _ ) ->
        --     ( model, Cmd.none, Effect.none )
        ( FlightTaskFormMsg flightTaskFormMsg, AddTask flightTaskForm ) ->
            let
                ( newFlightTaskForm, flightTaskFormCmd, flightTaskFormEffects ) =
                    FlightTaskForm.update flightTaskFormMsg flightTaskForm

                applyEffect eff =
                    case eff of
                        FlightTaskForm.FlightTaskSaved taskId ->
                            ( identity, Cmd.none, (FlightTaskSaved >> effect) taskId )
            in
            ( model |> withPage (AddTask newFlightTaskForm)
            , Cmd.map FlightTaskFormMsg flightTaskFormCmd
            , Effect.none
            )
                |> Effect.applyMapAll applyEffect flightTaskFormEffects

        ( FlightTaskFormMsg _, _ ) ->
            ( model, Cmd.none, Effect.none )

        ( FlightTrackUploadMsg flightTrackUploadMsg, UploadTrack flightTrackUpload ) ->
            let
                ( newFlightTrackUpload, flightTrackUploadCmd ) =
                    FlightTrackUpload.update flightTrackUploadMsg flightTrackUpload
            in
            ( model |> withPage (UploadTrack newFlightTrackUpload)
            , Cmd.map FlightTrackUploadMsg flightTrackUploadCmd
            , Effect.none
            )

        ( FlightTrackUploadMsg _, _ ) ->
            ( model, Cmd.none, Effect.none )


type alias Props =
    { navPoints : Deferred (ApiResult (List (Entity Int NavPoint)))
    , flightTasks : Deferred (ApiResult (List (Entity Int FlightTask)))
    }


view : Props -> Model -> Element Msg
view { navPoints, flightTasks } model =
    case model.page of
        SelectTask ->
            FlightTaskList.view
                { flightTasks = flightTasks
                , onTaskSelected = FlightTaskSelected
                , onCreateTaskTriggered = CreateTaskTriggered
                }

        AddTask flightTaskForm ->
            FlightTaskForm.view
                FlightTaskFormMsg
                { navPoints = navPoints
                , backTriggered = NavBackTriggered
                }
                flightTaskForm

        UploadTrack flightTrackUpload ->
            FlightTrackUpload.view
                FlightTrackUploadMsg
                { onBackTriggered = NavBackTriggered
                }
                flightTrackUpload
