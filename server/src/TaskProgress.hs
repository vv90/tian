module TaskProgress where

import Relude
import Data.Time (UTCTime)
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmType, HasElmEncoder, HasElmDecoder)
import Magic.ElmDeriving (ElmType)
import ProgressPoint (ProgressPoint, ProgressPointDto)
import qualified ProgressPoint

data TaskProgress = TaskProgress 
    { taskId :: Int32
    , date :: UTCTime
    , compId :: Text
    , points :: [ProgressPoint]
    }

data TaskProgressDto = TaskProgressDto
    { taskId :: Int32
    , date :: UTCTime
    , compId :: Text
    , points :: [ProgressPointDto]
    }
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.TaskProgress.TaskProgress" TaskProgressDto

toDto :: TaskProgress -> TaskProgressDto
toDto (TaskProgress taskId' date' compId' points') = TaskProgressDto
    { taskId = taskId'
    , date = date'
    , compId = compId'
    , points = ProgressPoint.toDto <$> points'
    }