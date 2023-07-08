module TimeUtils where

import Data.Time (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Relude

diffTimeToMillis :: DiffTime -> Int
diffTimeToMillis t =
  let picos = diffTimeToPicoseconds t
      millis = picos `div` 1000000000
   in fromIntegral millis

millisToDiffTime :: Int -> DiffTime
millisToDiffTime t =
  let picos :: Integer
      picos = fromIntegral t * 1000000000
   in picosecondsToDiffTime picos

diffTimeToHours :: DiffTime -> Double
diffTimeToHours t =
  let millis = diffTimeToMillis t
      hours :: Double
      hours = fromIntegral millis / 3600000
   in hours

diffTimeToSeconds :: DiffTime -> Double
diffTimeToSeconds t =
  let millis = diffTimeToMillis t
      seconds :: Double
      seconds = fromIntegral millis / 1000
   in seconds
