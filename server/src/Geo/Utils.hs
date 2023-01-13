module Geo.Utils where

import Relude
import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (S84)

s84position :: GeoPosition a => a -> Geodetic.HorizontalPosition S84
s84position pos = 
    let 
        (LatitudeDegrees lat) = latitude pos
        (LongitudeDegrees lon) = longitude pos
    in
    Geodetic.s84Pos lat lon

perpendicular :: Geodetic.HorizontalPosition S84 -> Angle.Angle -> Length.Length -> Maybe (GreatCircle.MinorArc S84)
perpendicular point bearing radius = 
    let 
        norm x = Angle.normalise x (Angle.decimalDegrees 360) 
    in
    GreatCircle.minorArc
        ( GreatCircle.destination 
            point
            (norm $ Angle.subtract bearing (Angle.decimalDegrees 90) )
            radius
        )
        ( GreatCircle.destination 
            point
            (norm $ Angle.add bearing (Angle.decimalDegrees 90))
            radius
        )
