module Common.ApiExtra exposing (..)

import Api.Geo exposing (latitudeDecoder, latitudeEncoder, longitudeDecoder, longitudeEncoder)
import Common.GeoUtils exposing (GeoPoint)
import Common.JsonCodecsExtra exposing (tupleDecoder, tupleEncoder)
import Json.Decode as D
import Json.Encode as E


geoPointEncoder : GeoPoint -> E.Value
geoPointEncoder ( lat, lon ) =
    tupleEncoder ( latitudeEncoder, longitudeEncoder ) ( lat, lon )


geoPointDecoder : D.Decoder GeoPoint
geoPointDecoder =
    tupleDecoder ( latitudeDecoder, longitudeDecoder )
