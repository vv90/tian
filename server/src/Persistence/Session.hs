module Persistence.Session where

import Data.Vector (Vector)
import Entity (Entity (..))
import FlightTask (FlightTask)
import Hasql.Session (Session, statement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import NavPoint (NavPoint)
import Persistence.Statement (deleteDuplicateNavPointsStatement, getAllFlightTasksStatement, getFlightTaskStatement, getNavPointsStatement, saveElevationPointsStatement, saveFlightTaskStatement, saveFlightTaskTurnpointsStatement, saveNavPointsStatement)
import Relude

getNavPointsSession :: Session (Vector NavPoint)
getNavPointsSession =
  statement () getNavPointsStatement

deleteDuplicateNavPointsSession :: Vector Text -> Session Int64
deleteDuplicateNavPointsSession names =
  statement names deleteDuplicateNavPointsStatement

saveNavPointsSession :: Vector NavPoint -> Session Int64
saveNavPointsSession nps =
  statement nps saveNavPointsStatement

saveElevationPointsSession :: (Int32, Double, Double) -> Session Int64
saveElevationPointsSession eps =
  statement eps saveElevationPointsStatement

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
