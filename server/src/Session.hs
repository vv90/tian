module Session 
    where

import Relude
import Data.Int (Int32)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Statement (getNavPointsStatement, saveNavPointsStatement, deleteDuplicateNavPointsStatement, saveFlightTaskStatement, saveFlightTaskTurnpointsStatement, getAllFlightTasksStatement, getFlightTaskStatement)
import NavPoint (NavPoint)
import Data.Vector (Vector)
import FlightTask (FlightTask)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (transaction, Mode (Write), IsolationLevel (Serializable))
import Entity (Entity(..))

getNavPointsSession :: Session (Vector NavPoint)
getNavPointsSession = 
    statement () getNavPointsStatement

deleteDuplicateNavPointsSession :: Vector Text -> Session Int64
deleteDuplicateNavPointsSession names =
    statement names deleteDuplicateNavPointsStatement

saveNavPointsSession :: Vector NavPoint -> Session Int64
saveNavPointsSession nps =
    statement nps saveNavPointsStatement

saveFlightTaskSession :: FlightTask -> Session Int64
saveFlightTaskSession ft = 
    transaction Serializable Write $ do
        taskId <- Transaction.statement ft saveFlightTaskStatement
        Transaction.statement (Entity taskId ft) saveFlightTaskTurnpointsStatement

getAllFlightTasksSession :: Session [Entity Int32 FlightTask]
getAllFlightTasksSession = 
    statement () getAllFlightTasksStatement

getFlightTaskSession :: Int32 -> Session (Maybe (Entity Int32 FlightTask))
getFlightTaskSession taskId = 
    statement taskId getFlightTaskStatement 

