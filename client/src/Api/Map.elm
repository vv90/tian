module Api.Map exposing
    ( GeoPoint
    , geoPointDecoder
    , geoPointEncoder
    )

import Api.Geo
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias GeoPoint =
    { lat : Api.Geo.Latitude, lon : Api.Geo.Longitude }


geoPointEncoder : GeoPoint -> Json.Encode.Value
geoPointEncoder a =
    Json.Encode.object
        [ ( "lat", Api.Geo.latitudeEncoder a.lat )
        , ( "lon", Api.Geo.longitudeEncoder a.lon )
        ]


geoPointDecoder : Json.Decode.Decoder GeoPoint
geoPointDecoder =
    Json.Decode.succeed GeoPoint
        |> Json.Decode.Pipeline.required "lat" Api.Geo.latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" Api.Geo.longitudeDecoder
