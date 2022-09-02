module Session 
    where

import Relude
import Data.Int (Int32)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Statement (getNavPointsStatement)
import NavPoint (NavPoint)
import Data.Vector (Vector)

getNavPointsSession :: Session (Either Text (Vector NavPoint))
getNavPointsSession = 
    statement () getNavPointsStatement

