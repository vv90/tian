module Geo.GeoUtils exposing 
  ( Longitude(..)
  , Latitude(..)
  , Altitude(..)
  , getAltitude
  , GeoPoint
  , showGeoPoint
  , getLon
  , getLat
  , Bearing
  , getBearing
  , bearingFromDeg
  , toMercatorWeb
  , fromMercatorWeb
  , toDecimalDegrees
  , distance
  , bearing
  , bearingDifference
  , addDeg
  , destination
  , lineIntersection
  , linePerpendicularToBearing
  , toSpherical
  , fromSpherical
  )

import Nav.Units exposing (..)
import Geo.Constants exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3, getX, getY, getZ, cross, normalize, negate)

-- function lon2tile(lon,zoom) { 
  -- return (Math.floor((lon+180)/360*Math.pow(2,zoom))); }
--  function lat2tile(lat,zoom)  { 
  -- return (Math.floor((1-Math.log(Math.tan(lat*Math.PI/180) + 1/Math.cos(lat*Math.PI/180))/Math.PI)/2 *Math.pow(2,zoom))); }

type Longitude = LonDeg Deg
type Latitude = LatDeg Deg
type Altitude = Altitude Meters

getAltitude : Altitude -> Meters
getAltitude (Altitude meters) =
  meters

type alias GeoPoint = 
  { lon: Longitude 
  , lat: Latitude
  }

showGeoPoint : GeoPoint -> String
showGeoPoint geoPoint =
  (getLon >> getDeg >> String.fromFloat) geoPoint.lon 
  ++ ", " 
  ++ (getLat >> getDeg >> String.fromFloat) geoPoint.lat
  

getLon : Longitude -> Deg
getLon (LonDeg lon) = lon

getLat : Latitude -> Deg
getLat (LatDeg lat) = lat

type Bearing = Bearing Deg

getBearing : Bearing -> Deg
getBearing (Bearing deg) = 
  deg

{-| Returns normalized bearing from 0 to 360 degrees
-}
bearingFromDeg : Deg -> Bearing
bearingFromDeg (Deg b) =
  if 
    b < 0 || b > 360
  then
    (Deg >> Bearing) (b - 360 * (truncate >> toFloat) (b / 360))
  else 
    (Deg >> Bearing) b

toMercatorWeb : GeoPoint -> (Float, Float)
toMercatorWeb ({lon, lat}) = 
  let
    latRad = lat |> (\(LatDeg latVal) -> degToRad latVal) |> getRad
    lonDeg = lon |> (\(LonDeg lonVal) -> getDeg lonVal)

    sec x = 1/cos x

    resY = logBase e (tan latRad + sec latRad)
  in
    ((lonDeg + 180) / 360, (1 - resY/pi) / 2)

-- Tile numbers to lon./lat.
-- n = 2 ^ zoom
-- lon_deg = xtile / n * 360.0 - 180.0
-- lat_rad = arctan(sinh(pi * (1 - 2 * ytile / n)))
-- lat_deg = lat_rad * 180.0 / pi
sinh : Float -> Float
sinh x = (e^x - e^ -x) / 2

fromMercatorWeb : (Float, Float) -> GeoPoint
fromMercatorWeb (x, y) =
  let
    
    lonDeg = x * 360 - 180
    latRad = atan (sinh (pi * (1 - 2 * y)))
    latDeg = latRad * 180 / pi
  in
    GeoPoint (LonDeg (Deg lonDeg)) (LatDeg (Deg latDeg))

{-| Converts from degrees, minutes, seconds to decimal degrees
-}
toDecimalDegrees : Int -> Float -> Float -> Float
toDecimalDegrees degrees minutes seconds =
  toFloat degrees + minutes/60 + seconds/3600

{-| Distance between 2 geo points
-}
distance : GeoPoint -> GeoPoint -> Meters
distance p1 p2 =
  let
    (lonRad1, latRad1) = 
      ( p1.lon |> getLon |> degToRad |> getRad
      , p1.lat |> getLat |> degToRad |> getRad
      )
    (lonRad2, latRad2) = 
      ( p2.lon |> getLon |> degToRad |> getRad
      , p2.lat |> getLat |> degToRad |> getRad
      )
    (deltaLon, deltaLat) = 
      (lonRad2 - lonRad1, latRad2 - latRad2)
    r = getMeters earthRadius
    -- a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
    a = sin(deltaLat/2) ^ 2 + cos(latRad1) * cos(latRad2) * sin(deltaLon/2) ^ 2
    -- c = 2 ⋅ atan2( √a, √(1−a) )
    c = 2 * atan2 (sqrt a) (sqrt (1-a))
    -- d = R ⋅ c
    d = r * c
    -- where	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
    -- note that angles need to be in radians to pass to trig functions!
  in
    Meters d

{-| Bearing from first point to the second
-}
bearing : GeoPoint -> GeoPoint -> Bearing
bearing p1 p2 =
  let
    (lonRad1, latRad1) = 
      ( p1.lon |> getLon |> degToRad |> getRad
      , p1.lat |> getLat |> degToRad |> getRad
      )
    (lonRad2, latRad2) = 
      ( p2.lon |> getLon |> degToRad |> getRad
      , p2.lat |> getLat |> degToRad |> getRad
      )
    (deltaLon, deltaLat) = 
      (lonRad2 - lonRad1, latRad2 - latRad2)
    x = sin deltaLon * cos latRad2
    y = cos latRad1 * sin latRad2 - sin latRad1 * cos latRad2 * cos deltaLon
    b = atan2 x y |> Rad |> radToDeg -- bearing between -180 and 180 degrees
  in
    bearingFromDeg b

{-| Returns the angle between 2 bearings (from 0 to 180 degrees)
-}
bearingDifference : Bearing -> Bearing -> Deg
bearingDifference (Bearing (Deg b1)) (Bearing (Deg b2)) =
  let
    angle = max b1 b2 - min b1 b2
  in
    if 
      (angle > 180)
    then 
      Deg (360 - angle)
    else 
      Deg angle

addDeg : Deg -> Deg -> Deg
addDeg (Deg x) (Deg y) = Deg (x + y)

{-| Calculates the destination geo point when traveling on a set bearing for a set distance from the start geo point
-}
destination : Bearing -> Meters -> GeoPoint -> GeoPoint
destination (Bearing b) (Meters d) p = 
  let
    (lonRad1, latRad1) = 
      ( p.lon |> getLon |> degToRad |> getRad
      , p.lat |> getLat |> degToRad |> getRad
      )
    bRad = b |> degToRad |> getRad
    r = getMeters earthRadius
    latRad2 = asin (sin latRad1 * cos (d/r) + cos latRad1 * sin (d/r) * cos bRad)
    lonRad2 = lonRad1 + atan2 (sin bRad * sin (d/r) * cos latRad1) (cos (d/r) - sin latRad1 * sin latRad2)
  in
    { lon = lonRad2 |> Rad |> radToDeg |> LonDeg 
    , lat = latRad2 |> Rad |> radToDeg |> LatDeg
    }

toSpherical : GeoPoint -> Vec3
toSpherical p = 
  let 
    lonRad = (getLon >> degToRad >> getRad) p.lon
    latRad = (getLat >> degToRad >> getRad) p.lat
  in
    vec3 
      (cos latRad * cos lonRad)
      (cos latRad * sin lonRad)
      (sin latRad)
    

fromSpherical : Vec3 -> GeoPoint
fromSpherical v =
  let
    latRad = asin (getZ v)
    tmp = cos latRad
    sign = if asin (getY v / tmp) > 0 then 1 else -1
    lonRad = acos (getX v / tmp) * sign
  in
    GeoPoint 
      ((Rad >> radToDeg >> LonDeg) lonRad) 
      ((Rad >> radToDeg >> LatDeg) latRad)

{-| Calculates a point of intersection of 2 lines (each represented by a tuple of 2 geo points)

    Returns `Nothing` if lines don't intersect
-}
lineIntersection : (GeoPoint, GeoPoint) -> (GeoPoint, GeoPoint) -> Maybe GeoPoint
lineIntersection (p11, p12) (p21, p22) =
-- todo: handle the case where all 4 points lie on the same great circle (i.e. collinear paths)
  let
    n1 = cross (toSpherical p11) (toSpherical p12)
    n2 = cross (toSpherical p21) (toSpherical p22)
    d = cross n1 n2 |> normalize
    s1 = fromSpherical d
    s2 = (V3.negate >> fromSpherical) d
    getDegLat = getLat >> getDeg
    getDegLon = getLon >> getDeg
    isBetweenE a b x = x >= min a b && x <= max a b
  in
    if 
      isBetweenE (getDegLat p11.lat) (getDegLat p12.lat) (getDegLat s1.lat) &&
      isBetweenE (getDegLat p21.lat) (getDegLat p22.lat) (getDegLat s1.lat) &&
      isBetweenE (getDegLon p11.lon) (getDegLon p12.lon) (getDegLon s1.lon) &&
      isBetweenE (getDegLon p21.lon) (getDegLon p22.lon) (getDegLon s1.lon)
    then 
      Just s1
    else if 
      isBetweenE (getDegLat p11.lat) (getDegLat p12.lat) (getDegLat s2.lat) &&
      isBetweenE (getDegLat p21.lat) (getDegLat p22.lat) (getDegLat s2.lat) &&
      isBetweenE (getDegLon p11.lon) (getDegLon p12.lon) (getDegLon s2.lon) &&
      isBetweenE (getDegLon p21.lon) (getDegLon p22.lon) (getDegLon s2.lon)
    then 
      Just s2
    else
      Nothing

{-| Returns a line of length `lineLength`, with midpoint of `origin`, 
perpendicular to the bearing of `bearing`
-}
linePerpendicularToBearing : Meters -> GeoPoint -> Bearing -> (GeoPoint, GeoPoint)
linePerpendicularToBearing (Meters lineLength) origin (Bearing b) =
  let
    radius = Meters (lineLength / 2)
    lp1 = destination (addDeg (Deg 90) b |> bearingFromDeg) radius origin
    lp2 = destination (addDeg (Deg -90) b |> bearingFromDeg) radius origin
  in
    (lp1, lp2)