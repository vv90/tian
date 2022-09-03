{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Statement
    where

import Relude
import Hasql.Statement (Statement, refineResult)
import Hasql.TH ( maybeStatement, vectorStatement, rowsAffectedStatement)
import Data.Text (Text)
import Data.Int (Int32)
import NavPoint (NavPoint (..), WaypointStyle, Latitude (LatitudeDegrees), Longitude (LongitudeDegrees), Elevation (ElevationMeters), Direction (DirectionDegrees), Length (LengthMeters), degreesLatitude, degreesLongitude, metersElevation, degreesDirection, metersLength)
import Data.Profunctor (dimap, Profunctor (lmap))
import Data.Vector (Vector)
import Data.Geo.Jord.Geodetic (latLongHeightPos)
import Data.Geo.Jord.Length (metres)
import Data.Geo.Jord.Models (WGS84(WGS84))
import Data.Geo.Jord.Angle (decimalDegrees)
import Control.Arrow (left)
import qualified Data.List as Vector
import qualified Hasql.Encoders as Encoders

newtype NavPointId = NavPointId Int32

-- getNavPointsStatement :: Statement () (Either Text (Vector NavPoint))
getNavPointsStatement :: Statement () (Vector NavPoint)
getNavPointsStatement =
    refineResult 
        (traverse makeNavPoint)
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
                descr :: text
            from nav_points
        |]
    where 
        makeNavPoint (name, code, country, lat, lon, elev, style, rwdir, rwlen, freq, descr) = do
            wpStyle <- left (\e -> "Failed to parse waypoint style for waypoint " <> code <> ": " <> e) $ readEither $ toString style 
            
            pure $ 
                NavPoint 
                    (toString name) 
                    (toString code) 
                    (toString <$> country) 
                    (LatitudeDegrees lat) 
                    (LongitudeDegrees lon) 
                    (ElevationMeters elev) 
                    wpStyle
                    (DirectionDegrees <$> rwdir) 
                    (LengthMeters <$> rwlen) 
                    (toString <$> freq) 
                    (toString descr)
        

deleteDuplicateNavPointsStatement :: Statement (Vector Text) Int64
deleteDuplicateNavPointsStatement = 
    [rowsAffectedStatement|
        delete from nav_points where code = any($1 :: text[])
    |]

saveNavPointsStatement :: Statement (Vector NavPoint) Int64
saveNavPointsStatement =
    lmap 
        nest
        [rowsAffectedStatement|
            insert into nav_points (name, code, country, lat, lon, elev, style, rwdir, rwlen, freq, descr) 
            select * from unnest ( $1 :: text[], $2 :: text[], $3 :: text?[], $4 :: float8[], $5 :: float8[], $6 :: float8[], $7 :: text[], $8 :: int4?[], $9 :: float8?[], $10 :: text?[], $11 :: text[] )
        |]
        -- lmap 
        --     nest
        --     [rowsAffectedStatement|
        --         insert into nav_points (name, country, lat)
        --         select * from unnest ($1 :: text[], $2 :: text?[], $3 :: float8[])
        --     |]
        --             values ($1 :: text, $2 :: text, $3 :: text?, $4 :: float8, $5 :: float8, $6 :: float8, $7 :: text, $8 :: int4?, $9 :: float8?, $10 :: text?, $11 :: text)
    where 
        nest nps =
            let 
                names = toText . name <$> nps
                codes = toText . code <$> nps
                countries = fmap toText . country <$> nps
                latitudes = degreesLatitude . lat <$> nps
                longitudes = degreesLongitude . lon <$> nps
                elevations = metersElevation . elev <$> nps
                styles = show . style <$> nps
                rwdirs = fmap degreesDirection . rwdir <$> nps
                rwlens = fmap metersLength . rwlen <$> nps
                freqs = fmap toText . freq <$> nps
                descs = toText . desc <$> nps
            in
                (names, codes, countries, latitudes, longitudes, elevations, styles, rwdirs, rwlens, freqs, descs)
    -- where 
    --     values = 
    --         fmap (\x -> 
    --             ( toText $ name x
    --             , toText $ code x
    --             , toText <$> country x
    --             , degreesLatitude $ lat x
    --             , degreesLongitude $ lon x
    --             , metersElevation $ elev x
    --             , show $ style x
    --             , degreesDirection <$> rwdir x
    --             , metersLength <$> rwlen x
    --             , toText <$> freq x
    --             , toText $ desc x
    --             )
    --         ) 