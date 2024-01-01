module Common.ApiCommands exposing (..)

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiResult exposing (ApiResult)
import Common.GeoUtils exposing (degreesLatitude, degreesLongitude, scaleLatitude, scaleLongitude, sumLatitude, sumLongitude)
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


loadElevationTileCmd : (ApiResult ElevationPointsTile -> msg) -> TileKey -> Cmd msg
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
        , expect = Http.expectJson onLoaded elevationPointsTileDecoder
        }


hydrateTile : ElevationPointsTile -> Array ( GeoPoint, Int )
hydrateTile tile =
    let
        makeGeoPoint : Int -> Int -> GeoPoint
        makeGeoPoint i j =
            { lat = sumLatitude tile.origin.lat (scaleLatitude (toFloat i) tile.latStep)
            , lon = sumLongitude tile.origin.lon (scaleLongitude (toFloat j) tile.lonStep)
            }

        -- xa =
        --     Array.initialize tile.rowLength (\rl -> Array.initialize tile.rowLength (\cl -> Array.get (rl * tile.rowLength + cl) points |> Maybe.map (Tuple.pair (makeGeoPoint cl rl))))
    in
    Array.indexedMap (\i elev -> ( makeGeoPoint (i // tile.rowLength) (modBy tile.rowLength i), elev )) tile.elevations
