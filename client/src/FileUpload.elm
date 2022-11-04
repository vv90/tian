module FileUpload exposing (..)

import Common.JsonCodecs exposing (filesDecoder)
import File exposing (File)
import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (on)
import Http
import Json.Decode as D
import Task exposing (perform)


type alias Model =
    String


init : () -> Model
init () =
    ""


type Msg
    = GotFiles (List File)
    | Uploaded (Result Http.Error ())
    | Print String


uploadFileCmd : List File -> Cmd Msg
uploadFileCmd files =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/navpoints"
        , headers = [] --[ Http.header "Content-Type" "multipart/form-data" ]
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectWhatever Uploaded
        , timeout = Nothing
        , tracker = Just "upload"
        }


showFilesCmd : List File -> Cmd Msg
showFilesCmd files =
    List.map (File.toString >> perform Print) files |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            ( model, uploadFileCmd files )

        -- ( model, showFilesCmd files )
        Uploaded (Ok _) ->
            ( "Success", Cmd.none )

        Uploaded (Err e) ->
            ( "Error: " ++ Debug.toString e, Cmd.none )

        Print s ->
            Debug.log s ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "File Upload demo" ]
        , input
            [ type_ "file"

            -- , multiple True
            , on "change" (D.map GotFiles filesDecoder)

            -- , onChange (\r -> GotFiles r)
            ]
            []
        , div [] [ text (Debug.toString model) ]
        ]
