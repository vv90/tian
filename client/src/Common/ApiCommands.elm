module Common.ApiCommands exposing (..)

import Api.Map exposing (Tile, tileEncoder)
import Common.ApiResult exposing (ApiResult)
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import Json.Encode as E
import MapUtils exposing (TileKey)


loadElevationsCmd : (ApiResult (List (List Int)) -> msg) -> List TileKey -> Cmd msg
loadElevationsCmd onLoaded tiles =
    let
        asTile ( x, y, z ) =
            Tile x y z
    in
    case tiles of
        [] ->
            Cmd.none

        _ ->
            Http.post
                { url = apiUrl "elevationPoints"
                , body = Http.jsonBody <| E.list tileEncoder (List.map asTile tiles)
                , expect = Http.expectJson onLoaded <| D.list <| D.list D.int
                }
