module Common.JsonCodecs exposing (..)

import File exposing (File)
import Json.Decode as D


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)
