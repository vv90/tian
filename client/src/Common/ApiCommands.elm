module Common.ApiCommands exposing (getCurrentFlights, getFlightInformation, hydrateTile, loadElevationTileCmd)

import Api.Types exposing (..)
import Array exposing (Array)
import Common.ApiResult exposing (ApiResult)
import Common.JsonCodecsExtra exposing (tupleDecoder)
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


getFlightInformation : String -> (ApiResult (Maybe FlightInformation) -> msg) -> Cmd msg
getFlightInformation deviceId onLoaded =
    Http.get
        { url = "/api/deviceInfo/" ++ deviceId
        , expect = Http.expectJson onLoaded <| Decode.maybe flightInformationDecoder
        }


getCurrentFlights : (ApiResult (List ( String, ( FlightInformation, FlightPosition ) )) -> msg) -> Cmd msg
getCurrentFlights onLoaded =
    let
        valueDecoder : Decode.Decoder ( FlightInformation, FlightPosition )
        valueDecoder =
            tupleDecoder ( flightInformationDecoder, flightPositionDecoder )
    in
    Http.get
        { url = "/api/currentFlights/"
        , expect = Http.expectJson onLoaded (Decode.list <| tupleDecoder ( Decode.string, valueDecoder ))
        }
