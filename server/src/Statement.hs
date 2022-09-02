{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Statement
    where

import Relude
import Hasql.Statement (Statement)
import Hasql.TH ( maybeStatement, vectorStatement)
import Data.Text (Text)
import Data.Int (Int32)
import NavPoint (NavPoint (..), WaypointStyle, Latitude (LatitudeDegrees), Longitude (LongitudeDegrees), Elevation (ElevationMeters), Direction (DirectionDegrees), Length (LengthMeters))
import Data.Profunctor (dimap)
import Data.Vector (Vector)
import Data.Geo.Jord.Geodetic (latLongHeightPos)
import Data.Geo.Jord.Length (metres)
import Data.Geo.Jord.Models (WGS84(WGS84))
import Data.Geo.Jord.Angle (decimalDegrees)
import Control.Arrow (left)

newtype NavPointId = NavPointId Int32

getNavPointsStatement :: Statement () (Either Text (Vector NavPoint))
getNavPointsStatement =
    traverse 
        (\(name, code, country, lat, lon, elev, style, rwdir, rwlen, freq, desc) -> 
            let 
                -- pos = latLongHeightPos lat lon (metres elev) WGS84
                wpStyle :: Either Text WaypointStyle
                wpStyle = left (\e -> "Failed to parse waypoint style for waypoint " <> code <> ": " <> e) $ readEither $ toString style 
            in
                (\s -> NavPoint name code country (LatitudeDegrees lat) (LongitudeDegrees lon) (ElevationMeters elev) s (DirectionDegrees <$> rwdir) (LengthMeters <$> rwlen) freq desc) <$> wpStyle
        )
    <$> 
    [vectorStatement|
        select 
            name :: text, 
            code :: text, 
            country :: text?, 
            lat :: float8,
            lon :: float8,
            elev :: float8,
            style :: text,
            rwdir :: int4?,
            rwlen :: float8?,
            freq :: text?,
            "desc" :: text
        from nav_points
    |]