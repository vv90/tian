module Page.FlightTrack.FlightTrackUpload exposing (..)

import Api.TaskProgress exposing (ProgressPoint, TaskProgress, taskProgressDecoder)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)
import Common.JsonCodecsExtra exposing (filesDecoder)
import Components.Player as Player
import Element exposing (Element, html, row, spacing, text)
import Element.Input as Input
import File exposing (File)
import Html exposing (input)
import Html.Attributes exposing (multiple, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as D


type alias Model =
    { taskId : Int
    , taskProgress : Deferred (ApiResult (List TaskProgress))
    , playerModel : Maybe Player.Model
    }


withPendingTaskProgress : Model -> Model
withPendingTaskProgress model =
    { model | taskProgress = setPending model.taskProgress }


init : Int -> Model
init taskId =
    { taskId = taskId
    , taskProgress = NotStarted
    , playerModel = Nothing
    }


type Msg
    = UploadTrack Int (List File) (AsyncOperationStatus (ApiResult (List TaskProgress)))
    | PlayerMsg Player.Msg



-- | PlaybackStarted
-- | PlaybackPaused
-- | PlaybackSpeedChanged Int


uploadTrackCmd : Int -> List File -> Cmd Msg
uploadTrackCmd taskId files =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/track/" ++ String.fromInt taskId
        , headers = []
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectJson (Finished >> UploadTrack taskId files) (D.list taskProgressDecoder)
        , timeout = Nothing
        , tracker = Just "upload"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadTrack taskId files Started ->
            ( model |> withPendingTaskProgress
            , uploadTrackCmd taskId files
            )

        UploadTrack _ _ (Finished result) ->
            ( { model
                | taskProgress = Resolved result
                , playerModel =
                    Result.toMaybe result
                        |> Maybe.andThen Player.init
              }
            , Cmd.none
            )

        PlayerMsg playerMsg ->
            case model.playerModel of
                Just playerModel ->
                    let
                        ( newPlayerModel, playerCmd ) =
                            Player.update playerMsg playerModel
                    in
                    ( { model | playerModel = Just newPlayerModel }
                    , Cmd.map PlayerMsg playerCmd
                    )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playerModel of
        Just pm ->
            Sub.map PlayerMsg (Player.subscriptions pm)

        Nothing ->
            Sub.none


type alias Props msg =
    { onBackTriggered : msg
    }



-- playbackView : PlaybackState -> Element Msg
-- playbackView state =
--     Input.button []
--         { onPress = Just }


view : (Msg -> msg) -> Props msg -> Model -> Element msg
view mapMsg { onBackTriggered } model =
    Element.column []
        [ row [ spacing 10 ]
            [ Input.button []
                { onPress = Just onBackTriggered
                , label = text "Back"
                }
            ]
        , case model.playerModel of
            Just playerModel ->
                Player.view playerModel |> Element.map (PlayerMsg >> mapMsg)

            Nothing ->
                html
                    (input
                        [ type_ "file"
                        , multiple True
                        , on "change" (D.map (\x -> UploadTrack model.taskId x Started |> mapMsg) filesDecoder)
                        ]
                        []
                    )
        ]
