module FlightTask where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import NavPoint (NavPoint (..))
import Relude

newtype Turnpoint
  = TurnpointCylinder Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Turnpoint" Turnpoint

newtype TaskStart
  = StartLine Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.TaskStart" TaskStart

data TaskFinish
  = FinishLine Double
  | FinishCylinder Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.TaskFinish" TaskFinish

data FlightTask = FlightTask
  { start :: (NavPoint, TaskStart),
    turnpoints :: [(NavPoint, Turnpoint)],
    finish :: (NavPoint, TaskFinish)
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.FlightTask" FlightTask

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
