module Common.ApiCommands exposing (hydrateTile, loadElevationTileCmd)

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiResult exposing (ApiResult)
import Domain.GeoUtils exposing (scaleLatitude, scaleLongitude, sumLatitude, sumLongitude)
import Env exposing (apiUrl)
import Http
import Tile exposing (TileKey)


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
    in
    Array.indexedMap (\i elev -> ( makeGeoPoint (i // tile.rowLength) (modBy tile.rowLength i), elev )) tile.elevations
