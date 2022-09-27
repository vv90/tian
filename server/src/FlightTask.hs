module FlightTask where

import Relude
import NavPoint (NavPoint(..))
import Generics.SOP qualified as SOP
import Data.Aeson qualified as Aeson
import Language.Haskell.To.Elm (HasElmType, HasElmEncoder, HasElmDecoder)
import Magic.ElmDeriving (ElmType)


newtype Turnpoint =
    Cylinder Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.FlightTask.Turnpoint" Turnpoint

newtype TaskStart =
    StartLine Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.FlightTask.TaskStart" TaskStart

data TaskFinish =
    FinishLine Double | FinishCylinder Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.FlightTask.TaskFinish" TaskFinish

data FlightTask =
    FlightTask
        { start :: (NavPoint, TaskStart)
        , turnpoints :: [(NavPoint, Turnpoint)]
        , finish :: (NavPoint, TaskFinish)
        }
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.FlightTask.FlightTask" FlightTask


-- makeFlightTask :: [NavPoint] -> (Text, TaskStart) -> [(Text, Turnpoint)] -> (Text, TaskFinish) -> Either Text FlightTask
-- makeFlightTask navPoints (startName, startLine) turnpointItems (finishName, finishLine) = do
--     startPoint <- findNavPoint startName
--     finishPoint <- findNavPoint finishName
--     turnpoints <- mapM makeTp turnpointItems

--     pure FlightTask
--         { start = (startPoint, startLine)
--         , turnpoints = turnpoints
--         , finish = (finishPoint, finishLine)
--         }
--     where
--         findNavPoint name = 
--             maybeToRight 
--                 ("NavPoint " <> name <> " not found") 
--                 (find (\NavPoint {name = n} -> n == name) navPoints)
        
--         makeTp :: (Text, Turnpoint) -> Either Text (NavPoint, Turnpoint)
--         makeTp (name, tp) = 
--             (,tp) <$> findNavPoint name
        

