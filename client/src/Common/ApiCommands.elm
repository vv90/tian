module Common.ApiCommands exposing (getDeviceInfo, hydrateTile, loadElevationTileCmd)

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiResult exposing (ApiResult)
import Domain.GeoUtils exposing (scaleLatitude, scaleLongitude, sumLatitude, sumLongitude)
import Http
import Json.Decode as Decode
import Tile exposing (TileKey)


loadElevationTileCmd : (ApiResult ElevationPointsTile -> msg) -> TileKey -> Cmd msg
loadElevationTileCmd onLoaded ( x, y, zoom ) =
    Http.get
        { url =
            "/tiles/v1/"
                ++ String.fromInt zoom
                ++ "/"
                ++ String.fromInt x
                ++ "_"
                ++ String.fromInt y
                ++ ".json"
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


getDeviceInfo : String -> (ApiResult (Maybe DeviceInfo) -> msg) -> Cmd msg
getDeviceInfo deviceId onLoaded =
    Http.get
        { url = "/api/deviceInfo/" ++ deviceId
        , expect = Http.expectJson onLoaded (Decode.maybe deviceInfoDecoder)
        }
