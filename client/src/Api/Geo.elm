module Api.Geo exposing
    ( Direction(..)
    , Distance(..)
    , Elevation(..)
    , Latitude(..)
    , Longitude(..)
    , directionDecoder
    , directionEncoder
    , distanceDecoder
    , distanceEncoder
    , elevationDecoder
    , elevationEncoder
    , latitudeDecoder
    , latitudeEncoder
    , longitudeDecoder
    , longitudeEncoder
    )

import Json.Decode
import Json.Encode


type Latitude
    = LatitudeDegrees Float


latitudeEncoder : Latitude -> Json.Encode.Value
latitudeEncoder a =
    case a of
        LatitudeDegrees b ->
            Json.Encode.float b


latitudeDecoder : Json.Decode.Decoder Latitude
latitudeDecoder =
    Json.Decode.map LatitudeDegrees Json.Decode.float


type Longitude
    = LongitudeDegrees Float


longitudeEncoder : Longitude -> Json.Encode.Value
longitudeEncoder a =
    case a of
        LongitudeDegrees b ->
            Json.Encode.float b


longitudeDecoder : Json.Decode.Decoder Longitude
longitudeDecoder =
    Json.Decode.map LongitudeDegrees Json.Decode.float


type Elevation
    = ElevationMeters Float


elevationEncoder : Elevation -> Json.Encode.Value
elevationEncoder a =
    case a of
        ElevationMeters b ->
            Json.Encode.float b


elevationDecoder : Json.Decode.Decoder Elevation
elevationDecoder =
    Json.Decode.map ElevationMeters Json.Decode.float


type Direction
    = DirectionDegrees Int


directionEncoder : Direction -> Json.Encode.Value
directionEncoder a =
    case a of
        DirectionDegrees b ->
            Json.Encode.int b


directionDecoder : Json.Decode.Decoder Direction
directionDecoder =
    Json.Decode.map DirectionDegrees Json.Decode.int


type Distance
    = DistanceMeters Float


distanceEncoder : Distance -> Json.Encode.Value
distanceEncoder a =
    case a of
        DistanceMeters b ->
            Json.Encode.float b


distanceDecoder : Json.Decode.Decoder Distance
distanceDecoder =
    Json.Decode.map DistanceMeters Json.Decode.float
