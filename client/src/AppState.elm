module AppState exposing
    ( Model
    , Msg(..)
    , getFlightTasksCmd
    , getNavPointsCmd
    , init
    , update
    , withPendingFlightTasks
    , withPendingNavPoints
    )

import Api.Types exposing (..)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)
import Env exposing (apiUrl)
import Http
import Json.Decode as D


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
        { url = apiUrl "navpoints"
        , expect = Http.expectJson (Finished >> GetNavPoints) (D.list <| entityDecoder D.int navPointDecoder)
        }


getFlightTasksCmd : Cmd Msg
getFlightTasksCmd =
    Http.get
        { url = apiUrl "task"
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
