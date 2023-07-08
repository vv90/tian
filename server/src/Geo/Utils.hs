module Geo.Utils where

import Data.Geo.Jord.Angle qualified as Angle
import Data.Geo.Jord.Geodetic qualified as Geodetic
import Data.Geo.Jord.GreatCircle qualified as GreatCircle
import Data.Geo.Jord.Length qualified as Length
import Data.Geo.Jord.Models (S84)
import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import Relude

s84position :: (GeoPosition a) => a -> Geodetic.HorizontalPosition S84
s84position pos =
  let (LatitudeDegrees lat) = latitude pos
      (LongitudeDegrees lon) = longitude pos
   in Geodetic.s84Pos lat lon

perpendicular :: Geodetic.HorizontalPosition S84 -> Angle.Angle -> Length.Length -> Maybe (GreatCircle.MinorArc S84)
perpendicular point bearing radius =
  let norm x = Angle.normalise x (Angle.decimalDegrees 360)
   in GreatCircle.minorArc
        ( GreatCircle.destination
            point
            (norm $ Angle.subtract bearing (Angle.decimalDegrees 90))
            radius
        )
        ( GreatCircle.destination
            point
            (norm $ Angle.add bearing (Angle.decimalDegrees 90))
            radius
        )
