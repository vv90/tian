module Persistence.Session where

import Data.Vector qualified as Vector
import Data.Vector (Vector)
import Entity (Entity (..))
import FlightTask (FlightTask)
import Hasql.Session (Session, statement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import NavPoint (NavPoint)
import Persistence.Statement (
  deleteDuplicateNavPointsStatement, 
  getAllFlightTasksStatement, 
  getFlightTaskStatement, 
  getNavPointsStatement, 
  -- saveElevationPointStatement, 
  saveSingleElevationPointStatement,
  checkImportedFileStatement,
  saveImportedFileStatement,
  saveFlightTaskStatement, 
  saveFlightTaskTurnpointsStatement, 
  saveNavPointsStatement )

import Relude
import GeoTiff.ElevationPoint (ElevationPoint (elevByte))
import Geo (latitude, longitude, degreesLongitude, degreesLatitude)

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
  let 
    -- saveGroup = flip Transaction.statement saveElevationPointStatement
    savePoint = flip Transaction.statement saveSingleElevationPointStatement

    pointToTuple :: ElevationPoint -> (Int16, Double, Double)
    pointToTuple p = (p.elevByte, degreesLongitude $ longitude p, degreesLatitude $ latitude p)

  in 
  transaction Serializable Write $ do
    existingFile <- Transaction.statement fileName checkImportedFileStatement

    case existingFile of
      Nothing -> do
        pts <- traverse (savePoint . pointToTuple) $ Vector.concat points
        _ <- Transaction.statement fileName saveImportedFileStatement
        
        pure $ Vector.sum pts
      Just _ ->
        pure 0