module Common.GeoUtils exposing (..)

import Api.Geo exposing (..)


metersElevation : Elevation -> Float
metersElevation (ElevationMeters meters) =
    meters


degreesDirection : Direction -> Int
degreesDirection (DirectionDegrees degrees) =
    degrees


degreesLongitude : Longitude -> Float
degreesLongitude (LongitudeDegrees degrees) =
    degrees


degreesLatitude : Latitude -> Float
degreesLatitude (LatitudeDegrees degrees) =
    degrees


metersDistance : Distance -> Float
metersDistance (DistanceMeters meters) =
    meters
