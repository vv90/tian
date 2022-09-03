module Session 
    where

import Relude
import Data.Int (Int32)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Statement (getNavPointsStatement, saveNavPointsStatement, deleteDuplicateNavPointsStatement)
import NavPoint (NavPoint)
import Data.Vector (Vector)

getNavPointsSession :: Session (Vector NavPoint)
getNavPointsSession = 
    statement () getNavPointsStatement

deleteDuplicateNavPointsSession :: Vector Text -> Session Int64
deleteDuplicateNavPointsSession names =
    statement names deleteDuplicateNavPointsStatement

saveNavPointsSession :: Vector NavPoint -> Session Int64
saveNavPointsSession nps =
    statement nps saveNavPointsStatement
