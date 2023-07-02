module Session where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import Entity (Entity (..))
import FlightTask (FlightTask)
import Hasql.Session (Session, statement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import NavPoint (NavPoint)
import Relude
import Statement (deleteDuplicateNavPointsStatement, getAllFlightTasksStatement, getFlightTaskStatement, getNavPointsStatement, saveFlightTaskStatement, saveFlightTaskTurnpointsStatement, saveNavPointsStatement)

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
