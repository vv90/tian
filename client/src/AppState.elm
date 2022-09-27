module AppState exposing (..)

import Api.Entity exposing (Entity, entityDecoder)
import Api.FlightTask exposing (FlightTask, flightTaskDecoder)
import Api.NavPoint exposing (NavPoint, navPointDecoder)
import Http
import Json.Decode as D
import Utils.ApiResult exposing (ApiResult)
import Utils.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)


type alias Model =
    { navPoints : Deferred (ApiResult (List NavPoint))
    , flightTasks : Deferred (ApiResult (List (Entity Int FlightTask)))
    }


init : Model
init =
    { navPoints = NotStarted
    , flightTasks = NotStarted
    }


withPendingNavPoints : Model -> Model
withPendingNavPoints model =
    { model | navPoints = setPending model.navPoints }


withPendingFlightTasks : Model -> Model
withPendingFlightTasks model =
    { model | flightTasks = setPending model.flightTasks }


type Msg
    = GetNavPoints (AsyncOperationStatus (ApiResult (List NavPoint)))
    | GetFlightTasks (AsyncOperationStatus (ApiResult (List (Entity Int FlightTask))))


getNavPointsCmd : Cmd Msg
getNavPointsCmd =
    Http.get
        { url = "http://0.0.0.0:8081/navpoints"
        , expect = Http.expectJson (Finished >> GetNavPoints) (D.list navPointDecoder)
        }


getFlightTasksCmd : Cmd Msg
getFlightTasksCmd =
    Http.get
        { url = "http://0.0.0.0:8081/task"
        , expect =
            Http.expectJson
                (Finished >> GetFlightTasks)
                (D.list <| entityDecoder D.int flightTaskDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNavPoints Started ->
            ( model |> withPendingNavPoints
            , getNavPointsCmd
            )

        GetNavPoints (Finished result) ->
            ( { model | navPoints = Resolved result }, Cmd.none )

        GetFlightTasks Started ->
            ( model |> withPendingFlightTasks
            , getFlightTasksCmd
            )

        GetFlightTasks (Finished result) ->
            ( { model | flightTasks = Resolved result }, Cmd.none )
