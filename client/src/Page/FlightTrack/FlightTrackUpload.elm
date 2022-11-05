module Page.FlightTrack.FlightTrackUpload exposing (..)

import Api.TaskProgress exposing (ProgressPoint)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (Deferred(..))
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
    , taskProgress : Deferred (ApiResult (List ProgressPoint))
    }


init : Int -> Model
init taskId =
    { taskId = taskId
    , taskProgress = NotStarted
    }


type Msg
    = GotFiles Int (List File)
    | Uploaded (Result Http.Error String)


uploadedFileCmd : Int -> List File -> Cmd Msg
uploadedFileCmd taskId files =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/track/" ++ String.fromInt taskId
        , headers = []
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectString Uploaded
        , timeout = Nothing
        , tracker = Just "upload"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles taskId files ->
            ( model, uploadedFileCmd taskId files )

        Uploaded (Ok _) ->
            ( model, Cmd.none )

        Uploaded (Err _) ->
            ( model, Cmd.none )


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
                , on "change" (D.map (mapMsg << GotFiles model.taskId) filesDecoder)
                ]
                []
            )
        ]
