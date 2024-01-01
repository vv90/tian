module Common.ApiCommands exposing (..)

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiResult exposing (ApiResult)
import Common.JsonCodecsExtra exposing (tripleDecoder, tupleDecoder, tupleEncoder)
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import Json.Encode as E
import Tile exposing (TileKey)


loadElevationsCmd : (ApiResult (List (Array (Array ( GeoPoint, Int )))) -> msg) -> List ( GeoPoint, GeoPoint ) -> Cmd msg
loadElevationsCmd onLoaded tiles =
    case tiles of
        [] ->
            Cmd.none

        _ ->
            Http.post
                { url = apiUrl "elevationPoints"
                , body = Http.jsonBody <| E.list (tupleEncoder ( geoPointEncoder, geoPointEncoder )) tiles
                , expect = Http.expectJson onLoaded <| D.list <| D.array <| D.array (tupleDecoder ( geoPointDecoder, D.int ))
                }


loadElevationTileCmd : (ApiResult (Array (Array ( GeoPoint, Int ))) -> msg) -> TileKey -> Cmd msg
loadElevationTileCmd onLoaded ( x, y, zoom ) =
    Http.get
        { url =
            apiUrl <|
                "elevationTile/"
                    ++ String.fromInt zoom
                    ++ "/"
                    ++ String.fromInt x
                    ++ "/"
                    ++ String.fromInt y
        , expect =
            tripleDecoder ( latitudeDecoder, longitudeDecoder, D.int )
                |> D.map (\( lat, lon, elev ) -> ( { lat = lat, lon = lon }, elev ))
                |> D.array
                |> D.array
                |> Http.expectJson onLoaded
        }
