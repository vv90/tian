{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Statement
    where

import Relude
import Hasql.Statement (Statement)
import Hasql.TH ( maybeStatement, vectorStatement)
import Data.Text (Text)
import Data.Int (Int32)
import NavPoint (NavPoint (..))
import Data.Profunctor (dimap)
import Data.Vector (Vector)

newtype NavPointId = NavPointId Int32

-- getNavPointsStatement :: Statement () (Vector NavPoint)
-- getNavPointsStatement =
--     (\(name, lat, lon) -> NavPoint name lat lon) 
--     <<$>>
--     [vectorStatement|
--         select name :: text, lat :: float8, lon :: float8 
--         from nav_points
--     |]