module AppState exposing (..)

import Api.Entity exposing (Entity, entityDecoder)
import Api.FlightTask exposing (FlightTask, flightTaskDecoder, flightTaskEncoder)
import Api.NavPoint exposing (NavPoint, navPointDecoder)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)
import Http
import Json.Decode as D
import Task exposing (Task)


type alias Model =
    { navPoints : Deferred (ApiResult (List (Entity Int NavPoint)))
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


withFlightTasks : ApiResult (List (Entity Int FlightTask)) -> Model -> Model
withFlightTasks result model =
    case ( result, model.flightTasks ) of
        ( Ok fts, _ ) ->
            { model | flightTasks = Resolved <| Ok fts }

        ( Err err, NotStarted ) ->
            { model | flightTasks = Resolved <| Err err }

        ( Err err, InProgress ) ->
            { model | flightTasks = Resolved <| Err err }

        ( Err _, Updating _ ) ->
            model

        ( Err _, Resolved _ ) ->
            model


type Msg
    = GetNavPoints (AsyncOperationStatus (ApiResult (List (Entity Int NavPoint))))
    | GetFlightTasks (AsyncOperationStatus (ApiResult (List (Entity Int FlightTask))))


getNavPointsCmd : Cmd Msg
getNavPointsCmd =
    Http.get
        { url = "http://0.0.0.0:8081/navpoints"
        , expect = Http.expectJson (Finished >> GetNavPoints) (D.list <| entityDecoder D.int navPointDecoder)
        }


jsonResponse : D.Decoder a -> Http.Response String -> Result Http.Error a
jsonResponse decoder response =
    case response of
        Http.BadUrl_ s ->
            Err <| Http.BadUrl s

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ meta body ->
            Err <| Http.BadStatus meta.statusCode

        Http.GoodStatus_ meta body ->
            body
                |> D.decodeString decoder
                |> Result.mapError (D.errorToString >> Http.BadBody)


saveFlightTaskTask : FlightTask -> Task Http.Error ( Int, List (Entity Int FlightTask) )
saveFlightTaskTask flightTask =
    Http.task
        { method = "POST"
        , url = "http://0.0.0.0:8081/task"
        , headers = []
        , body = Http.jsonBody <| flightTaskEncoder flightTask
        , timeout = Nothing
        , resolver = Http.stringResolver (jsonResponse D.int)
        }
        |> Task.andThen
            (\ftId ->
                Task.map (Tuple.pair ftId) getFlightTasksTask
            )


getFlightTasksTask : Task Http.Error (List (Entity Int FlightTask))
getFlightTasksTask =
    Http.task
        { method = "GET"
        , url = "http://0.0.0.0:8081/task"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver =
            Http.stringResolver <|
                jsonResponse <|
                    D.list <|
                        entityDecoder D.int flightTaskDecoder
        }


saveFlightTaskCmd : (ApiResult Int -> msg) -> FlightTask -> Cmd msg
saveFlightTaskCmd toMsg flightTask =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/task"
        , headers = []
        , body = Http.jsonBody <| flightTaskEncoder flightTask
        , expect = Http.expectJson toMsg D.int
        , timeout = Nothing
        , tracker = Nothing
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
            ( model |> withFlightTasks result
            , Cmd.none
            )
