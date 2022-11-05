module TaskProgress where

import Relude
import Data.Time (UTCTime)
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmType, HasElmEncoder, HasElmDecoder)
import Magic.ElmDeriving (ElmType)
import ProgressPoint (ProgressPoint)

data TaskProgress = TaskProgress 
    { taskId :: Int32
    , date :: UTCTime
    , compId :: Text
    , points :: [ProgressPoint]
    }
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.TaskProgress.TaskProgress" TaskProgress