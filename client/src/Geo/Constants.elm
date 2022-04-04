module Geo.Constants exposing (..)
import Nav.Units exposing (..)

metersPerPixel : Int -> Maybe Meters
metersPerPixel zoom =
  let 
    mpp = 
      case zoom of
        0 -> Just 156412
        1 -> Just 78206
        2 -> Just 39103
        3 -> Just 19551
        4 -> Just 9776
        5 -> Just 4888
        6 -> Just 2444
        7 -> Just 1222
        8 -> Just 610.984
        9 -> Just 305.492
        10 -> Just 152.746
        11 -> Just 76.373
        12 -> Just 38.187
        13 -> Just 19.093
        14 -> Just 9.547
        15 -> Just 4.773
        16 -> Just 2.387
        17 -> Just 1.193
        18 -> Just 0.596
        19 -> Just 0.298
        20 -> Just 0.149
        _ -> Nothing
  in  
    Maybe.map Meters mpp

earthRadius : Meters
earthRadius = Meters 6372798.2