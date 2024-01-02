module Page.Test.TestProgress exposing
    ( Model
    , Msg(..)
    , init
    , update
    )

import Api.Types exposing (..)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredToMaybe, setPending)
import Common.JsonCodecsExtra exposing (tupleDecoder, tupleEncoder)
import Domain.GeoUtils exposing (degreesLatitude, degreesLongitude)
import Element exposing (Element, column, spacing, text)
import Element.Input as Input
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Nonempty as NE exposing (Nonempty)
import MapUtils exposing (LineStyle(..), MapItem(..))
import Maybe.Extra as MaybeX


type alias Model =
    { progress : Deferred (ApiResult TaskProgress)
    , startLine : Deferred (ApiResult ( GeoPoint, GeoPoint ))
    }


type Msg
    = UpdateProgress Int (Nonempty GeoPoint) (AsyncOperationStatus (ApiResult TaskProgress))
    | GotStartLine (ApiResult ( GeoPoint, GeoPoint ))
    | Reset


init : Model
init =
    { progress = NotStarted
    , startLine = NotStarted
    }


toMapItems : Model -> List MapItem
toMapItems model =
    let
        progressPointItem : ProgressPoint -> Maybe MapItem
        progressPointItem p =
            Maybe.map
                (Marker { lat = p.lat, lon = p.lon })
                p.target

        taskProgressItems : List MapItem
        taskProgressItems =
            deferredToMaybe model.progress
                |> Maybe.andThen Result.toMaybe
                |> Maybe.andThen (.points >> List.map progressPointItem >> MaybeX.combine)
                |> Maybe.withDefault []

        startLineItems : List MapItem
        startLineItems =
            deferredToMaybe model.startLine
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map (\( p1, p2 ) -> [ Line TaskLine [ p1, p2 ] ])
                |> Maybe.withDefault []
    in
    taskProgressItems ++ startLineItems


updateProgressCmd : Int -> Nonempty GeoPoint -> Cmd Msg
updateProgressCmd taskId points =
    Http.request
        { method = "POST"
        , url = apiUrl "test/taskProgress/" ++ String.fromInt taskId
        , headers = []
        , body = Http.jsonBody <| E.list geoPointEncoder (NE.toList points)
        , expect = Http.expectJson (UpdateProgress taskId points << Finished) taskProgressDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getTaskStartLine : Int -> Cmd Msg
getTaskStartLine taskId =
    Http.request
        { method = "GET"
        , url = apiUrl "test/startLine/" ++ String.fromInt taskId
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson GotStartLine (tupleDecoder ( geoPointDecoder, geoPointDecoder ))
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateProgress taskId pts status ->
            case status of
                Started ->
                    ( { model | progress = setPending model.progress }, updateProgressCmd taskId pts )

                Finished res ->
                    ( { model | progress = Resolved res }, getTaskStartLine taskId )

        Reset ->
            ( init, Cmd.none )

        GotStartLine res ->
            ( { model | startLine = Resolved res }, Cmd.none )


view : Model -> Element Msg
view model =
    let
        viewPoint : ProgressPoint -> Element Msg
        viewPoint p =
            text <|
                (degreesLatitude >> String.fromFloat) p.lat
                    ++ " "
                    ++ (degreesLongitude >> String.fromFloat) p.lon
                    ++ " "
                    ++ Maybe.withDefault " - " p.target

        points : List (Element Msg)
        points =
            deferredToMaybe model.progress
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map (.points >> List.map viewPoint)
                |> Maybe.withDefault []

        resetBtn : Element Msg
        resetBtn =
            Input.button
                []
                { onPress = Just Reset
                , label = text "Reset"
                }
    in
    column
        [ spacing 10 ]
        (points ++ [ resetBtn ])
