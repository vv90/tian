module Persistence.Session where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Entity (Entity (..))
import FlightTask (FlightTask)
-- saveElevationPointStatement,

import Geo (degreesLatitude, degreesLongitude, latitude, longitude)
import GeoTiff.ElevationPoint (ElevationPoint (elevByte))
import Hasql.Session (Session, statement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import NavPoint (NavPoint)
import Persistence.Statement
  ( ElevationPointQuery,
    checkImportedFileStatement,
    deleteDuplicateNavPointsStatement,
    getAllFlightTasksStatement,
    getElevationPointsStatement,
    getFlightTaskStatement,
    getNavPointsStatement,
    saveFlightTaskStatement,
    saveFlightTaskTurnpointsStatement,
    saveImportedFileStatement,
    saveNavPointsStatement,
    saveSingleElevationPointStatement,
  )
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

saveElevationPointsSession :: Text -> [Vector ElevationPoint] -> Session Int64
saveElevationPointsSession fileName points =
  let -- saveGroup = flip Transaction.statement saveElevationPointStatement
      savePoint = flip Transaction.statement saveSingleElevationPointStatement

      pointToTuple :: ElevationPoint -> (Int16, Double, Double)
      pointToTuple p = (p.elevByte, degreesLongitude $ longitude p, degreesLatitude $ latitude p)
   in transaction Serializable Write $ do
        existingFile <- Transaction.statement fileName checkImportedFileStatement

        case existingFile of
          Nothing -> do
            pts <- traverse (savePoint . pointToTuple) $ Vector.concat points
            _ <- Transaction.statement fileName saveImportedFileStatement

            pure $ Vector.sum pts
          Just _ ->
            pure 0

getElevationPointsSession :: ElevationPointQuery -> Session (Vector (Double, Double, Double))
getElevationPointsSession query =
  statement query getElevationPointsStatement
