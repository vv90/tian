module Page.FlightTrack.FlightTrackUpload exposing (..)

import Element exposing (Element, html)
import File exposing (File)
import Html exposing (input)
import Html.Attributes exposing (multiple, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as D
import Utils.JsonCodecs exposing (filesDecoder)


type alias Model =
    String


init : Model
init =
    ""


type Msg
    = GotFiles (List File)
    | Uploaded (Result Http.Error String)


uploadedFileCmd : List File -> Cmd Msg
uploadedFileCmd files =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/track"
        , headers = []
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectString Uploaded
        , timeout = Nothing
        , tracker = Just "upload"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            ( model, uploadedFileCmd files )

        Uploaded (Ok s) ->
            ( s, Cmd.none )

        Uploaded (Err _) ->
            ( "Error", Cmd.none )


view : Model -> Element Msg
view model =
    Element.column []
        [ Element.text model
        , html
            (input
                [ type_ "file"
                , multiple True
                , on "change" (D.map GotFiles filesDecoder)
                ]
                []
            )
        ]
