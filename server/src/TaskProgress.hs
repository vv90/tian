module TaskProgress where

import Data.Aeson qualified as Aeson
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Models (S84)
import Data.Time (UTCTime)
import Generics.SOP qualified as SOP
import Geo (GeoPosition (latitude, longitude), Latitude, Longitude)
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import ProgressPoint (ProgressPoint, ProgressPointDto)
import ProgressPoint qualified
import Relude

data TaskProgress = TaskProgress
  { taskId :: Int32,
    date :: UTCTime,
    compId :: Text,
    points :: [ProgressPoint],
    legs :: [HorizontalPosition S84]
  }

data TaskProgressDto = TaskProgressDto
  { taskId :: Int32,
    date :: UTCTime,
    compId :: Text,
    points :: [ProgressPointDto],
    legs :: [(Latitude, Longitude)]
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.TaskProgress.TaskProgress" TaskProgressDto

toDto :: TaskProgress -> TaskProgressDto
toDto (TaskProgress taskId' date' compId' points' legs') =
  TaskProgressDto
    { taskId = taskId',
      date = date',
      compId = compId',
      points = ProgressPoint.toDto <$> points',
      legs = (\pos -> (latitude pos, longitude pos)) <$> legs'
    }
