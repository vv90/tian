module Page.FlightTrack.FlightTrackUpload exposing (..)

import Api.TaskProgress exposing (ProgressPoint, TaskProgress, taskProgressDecoder)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), setPending)
import Common.JsonCodecs exposing (filesDecoder)
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
    }


withPendingTaskProgress : Model -> Model
withPendingTaskProgress model =
    { model | taskProgress = setPending model.taskProgress }


init : Int -> Model
init taskId =
    { taskId = taskId
    , taskProgress = NotStarted
    }


type Msg
    = UploadTrack Int (List File) (AsyncOperationStatus (ApiResult (List TaskProgress)))


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
            ( { model | taskProgress = Resolved result }
            , Cmd.none
            )


type alias Props msg =
    { onBackTriggered : msg
    }


view : (Msg -> msg) -> Props msg -> Model -> Element msg
view mapMsg { onBackTriggered } model =
    Element.column []
        [ row [ spacing 10 ]
            [ Input.button []
                { onPress = Just onBackTriggered
                , label = text "Back"
                }
            ]
        , html
            (input
                [ type_ "file"
                , multiple True
                , on "change" (D.map (\x -> UploadTrack model.taskId x Started |> mapMsg) filesDecoder)
                ]
                []
            )
        ]
