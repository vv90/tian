module Common.ApiCommands exposing (..)

import Api.Map exposing (GeoPoint, geoPointDecoder, geoPointEncoder)
import Common.ApiResult exposing (ApiResult)
import Common.JsonCodecsExtra exposing (tupleDecoder, tupleEncoder)
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import Json.Encode as E


loadElevationsCmd : (ApiResult (List (List ( GeoPoint, Float ))) -> msg) -> List ( GeoPoint, GeoPoint ) -> Cmd msg
loadElevationsCmd onLoaded tiles =
    case tiles of
        [] ->
            Cmd.none

        _ ->
            Http.post
                { url = apiUrl "elevationPoints"
                , body = Http.jsonBody <| E.list (tupleEncoder ( geoPointEncoder, geoPointEncoder )) tiles
                , expect = Http.expectJson onLoaded <| D.list <| D.list (tupleDecoder ( geoPointDecoder, D.float ))
                }
