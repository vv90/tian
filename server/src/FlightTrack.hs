module FlightTrack where

import Data.Time
import Relude
import TrackPoint (TrackPoint)

data FlightTrack = FlightTrack
  { date :: UTCTime,
    compId :: Text,
    points :: NonEmpty TrackPoint
  }
  deriving (Show, Eq)
