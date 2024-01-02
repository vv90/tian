module Domain.GeoUtils exposing
    ( Bearing
    , bearing
    , degreesLatitude
    , degreesLongitude
    , fromMercatorWeb
    , linePerpendicularToBearing
    , metersDistance
    , metersElevation
    , scaleLatitude
    , scaleLongitude
    , sumLatitude
    , sumLongitude
    , toMercatorWeb
    )

import Api.Types exposing (..)
import Common.Utils exposing (sinh)


type Bearing
    = Bearing Float


earthRadius : Distance
earthRadius =
    DistanceMeters 6372798.2


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


sumLatitude : Latitude -> Latitude -> Latitude
sumLatitude (LatitudeDegrees lat1) (LatitudeDegrees lat2) =
    LatitudeDegrees (lat1 + lat2)


sumLongitude : Longitude -> Longitude -> Longitude
sumLongitude (LongitudeDegrees lon1) (LongitudeDegrees lon2) =
    LongitudeDegrees (lon1 + lon2)


scaleLatitude : Float -> Latitude -> Latitude
scaleLatitude scale (LatitudeDegrees lat) =
    LatitudeDegrees (lat * scale)


scaleLongitude : Float -> Longitude -> Longitude
scaleLongitude scale (LongitudeDegrees lon) =
    LongitudeDegrees (lon * scale)


metersDistance : Distance -> Float
metersDistance (DistanceMeters meters) =
    meters


toMercatorWeb : GeoPoint -> ( Float, Float )
toMercatorWeb { lat, lon } =
    let
        latRad : Float
        latRad =
            -- lat |> (\(LatDeg latVal) -> degToRad latVal) |> getRad
            lat |> degreesLatitude |> degrees

        lonDeg : Float
        lonDeg =
            -- lon |> (\(LonDeg lonVal) -> getDeg lonVal)
            lon |> degreesLongitude

        sec : Float -> Float
        sec x =
            1 / cos x

        resY : Float
        resY =
            logBase e (tan latRad + sec latRad)
    in
    ( (lonDeg + 180) / 360, (1 - resY / pi) / 2 )


fromMercatorWeb : ( Float, Float ) -> GeoPoint
fromMercatorWeb ( x, y ) =
    let
        lonDeg : Float
        lonDeg =
            x * 360 - 180

        latRad : Float
        latRad =
            atan (sinh (pi * (1 - 2 * y)))

        latDeg : Float
        latDeg =
            latRad * 180 / pi
    in
    { lat = LatitudeDegrees latDeg, lon = LongitudeDegrees lonDeg }



-- showGeoPoint : GeoPoint -> String
-- showGeoPoint geoPoint =
--     (getLon >> getDeg >> String.fromFloat) geoPoint.lon
--         ++ ", "
--         ++ (getLat >> getDeg >> String.fromFloat) geoPoint.lat


{-| Returns normalized bearing from 0 to 360 degrees
-}
normalizedBearing : Float -> Bearing
normalizedBearing b =
    if b < 0 || b > 360 then
        Bearing <| b - 360 * (truncate >> toFloat) (b / 360)

    else
        Bearing b


radToDeg : Float -> Float
radToDeg x =
    x * 180.0 / pi



-- Tile numbers to lon./lat.
-- n = 2 ^ zoom
-- lon_deg = xtile / n * 360.0 - 180.0
-- lat_rad = arctan(sinh(pi * (1 - 2 * ytile / n)))
-- lat_deg = lat_rad * 180.0 / pi


{-| Converts from degrees, minutes, seconds to decimal degrees
-}
toDecimalDegrees : Int -> Float -> Float -> Float
toDecimalDegrees degrees minutes seconds =
    toFloat degrees + minutes / 60 + seconds / 3600


{-| Distance between 2 geo points
-}
distance : GeoPoint -> GeoPoint -> Distance
distance gp1 gp2 =
    let
        ( p1Lat, p1Lon ) =
            ( gp1.lat, gp1.lon )

        ( p2Lat, p2Lon ) =
            ( gp2.lat, gp2.lon )

        ( lonRad1, latRad1 ) =
            ( p1Lon |> degreesLongitude |> degrees
            , p1Lat |> degreesLatitude |> degrees
            )

        ( lonRad2, latRad2 ) =
            ( p2Lon |> degreesLongitude |> degrees
            , p2Lat |> degreesLatitude |> degrees
            )

        ( deltaLon, deltaLat ) =
            ( lonRad2 - lonRad1, latRad2 - latRad2 )

        r =
            metersDistance earthRadius

        -- a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
        a =
            sin (deltaLat / 2) ^ 2 + cos latRad1 * cos latRad2 * sin (deltaLon / 2) ^ 2

        -- c = 2 ⋅ atan2( √a, √(1−a) )
        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))

        -- d = R ⋅ c
        d =
            r * c

        -- where	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
        -- note that angles need to be in radians to pass to trig functions!
    in
    DistanceMeters d


{-| Bearing from first point to the second
-}
bearing : GeoPoint -> GeoPoint -> Bearing
bearing gp1 gp2 =
    let
        ( p1Lat, p1Lon ) =
            ( gp1.lat, gp1.lon )

        ( p2Lat, p2Lon ) =
            ( gp2.lat, gp2.lon )

        ( lonRad1, latRad1 ) =
            ( p1Lon |> degreesLongitude |> degrees
            , p1Lat |> degreesLatitude |> degrees
            )

        ( lonRad2, latRad2 ) =
            ( p2Lon |> degreesLongitude |> degrees
            , p2Lat |> degreesLatitude |> degrees
            )

        ( deltaLon, deltaLat ) =
            ( lonRad2 - lonRad1, latRad2 - latRad2 )

        x =
            sin deltaLon * cos latRad2

        y =
            cos latRad1 * sin latRad2 - sin latRad1 * cos latRad2 * cos deltaLon

        -- bearing between -180 and 180 degrees
        b =
            atan2 x y |> radToDeg
    in
    normalizedBearing b


{-| Returns the angle between 2 bearings (from 0 to 180 degrees)
-}
bearingDifference : Bearing -> Bearing -> Float
bearingDifference (Bearing b1) (Bearing b2) =
    let
        angle =
            max b1 b2 - min b1 b2
    in
    if angle > 180 then
        360 - angle

    else
        angle


{-| Calculates the destination geo point when traveling on a set bearing for a set distance from the start geo point
-}
destination : Bearing -> Distance -> GeoPoint -> GeoPoint
destination (Bearing b) (DistanceMeters d) { lat, lon } =
    let
        ( lonRad1, latRad1 ) =
            ( lon |> degreesLongitude |> degrees
            , lat |> degreesLatitude |> degrees
            )

        bRad =
            degrees b

        r =
            metersDistance earthRadius

        latRad2 =
            asin (sin latRad1 * cos (d / r) + cos latRad1 * sin (d / r) * cos bRad)

        lonRad2 =
            lonRad1 + atan2 (sin bRad * sin (d / r) * cos latRad1) (cos (d / r) - sin latRad1 * sin latRad2)
    in
    { lat = latRad2 |> radToDeg |> LatitudeDegrees
    , lon = lonRad2 |> radToDeg |> LongitudeDegrees
    }


{-| Returns a line with midpoint of `origin`, perpendicular to the bearing of `bearing` of length `2 * radius`
-}
linePerpendicularToBearing : Distance -> GeoPoint -> Bearing -> ( GeoPoint, GeoPoint )
linePerpendicularToBearing radius origin (Bearing b) =
    let
        bLeft =
            normalizedBearing (b - 90)

        bRight =
            normalizedBearing (b + 90)

        lp1 =
            destination bLeft radius origin

        lp2 =
            destination bRight radius origin
    in
    ( lp1, lp2 )



-- toSpherical : GeoPoint -> Vec3
-- toSpherical p =
--     let
--         lonRad =
--             (getLon >> degToRad >> getRad) p.lon
--         latRad =
--             (getLat >> degToRad >> getRad) p.lat
--     in
--     vec3
--         (cos latRad * cos lonRad)
--         (cos latRad * sin lonRad)
--         (sin latRad)
-- fromSpherical : Vec3 -> GeoPoint
-- fromSpherical v =
--     let
--         latRad =
--             asin (getZ v)
--         tmp =
--             cos latRad
--         sign =
--             if asin (getY v / tmp) > 0 then
--                 1
--             else
--                 -1
--         lonRad =
--             acos (getX v / tmp) * sign
--     in
--     GeoPoint
--         ((Rad >> radToDeg >> LonDeg) lonRad)
--         ((Rad >> radToDeg >> LatDeg) latRad)
-- {-| Calculates a point of intersection of 2 lines (each represented by a tuple of 2 geo points)
--    Returns `Nothing` if lines don't intersect
--}



-- lineIntersection : ( GeoPoint, GeoPoint ) -> ( GeoPoint, GeoPoint ) -> Maybe GeoPoint
-- lineIntersection ( p11, p12 ) ( p21, p22 ) =
--     -- todo: handle the case where all 4 points lie on the same great circle (i.e. collinear paths)
--     let
--         n1 =
--             cross (toSpherical p11) (toSpherical p12)
--         n2 =
--             cross (toSpherical p21) (toSpherical p22)
--         d =
--             cross n1 n2 |> normalize
--         s1 =
--             fromSpherical d
--         s2 =
--             (V3.negate >> fromSpherical) d
--         getDegLat =
--             getLat >> getDeg
--         getDegLon =
--             getLon >> getDeg
--         isBetweenE a b x =
--             x >= min a b && x <= max a b
--     in
--     if
--         isBetweenE (getDegLat p11.lat) (getDegLat p12.lat) (getDegLat s1.lat)
--             && isBetweenE (getDegLat p21.lat) (getDegLat p22.lat) (getDegLat s1.lat)
--             && isBetweenE (getDegLon p11.lon) (getDegLon p12.lon) (getDegLon s1.lon)
--             && isBetweenE (getDegLon p21.lon) (getDegLon p22.lon) (getDegLon s1.lon)
--     then
--         Just s1
--     else if
--         isBetweenE (getDegLat p11.lat) (getDegLat p12.lat) (getDegLat s2.lat)
--             && isBetweenE (getDegLat p21.lat) (getDegLat p22.lat) (getDegLat s2.lat)
--             && isBetweenE (getDegLon p11.lon) (getDegLon p12.lon) (getDegLon s2.lon)
--             && isBetweenE (getDegLon p21.lon) (getDegLon p22.lon) (getDegLon s2.lon)
--     then
--         Just s2
--     else
--         Nothing
