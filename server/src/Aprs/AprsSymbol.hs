module Aprs.AprsSymbol where

import Relude

data AprsSymbol
  = Glider
  | SmallPlane
  | LargePlane
  | Helicopter
  | Parachute
  | Baloon
  | GroundVehicle
  | StaticObject
  | Unknown
  deriving stock (Show, Eq)

-- FLRDDE626>APRS,qAS,EGHL:/074548h5111.32N/00102.04W'086/007/A=000607 id0ADDE626 -019fpm +0.0rot 5.5dB 3e -4.3kHz
-- The APRS symbols are the ones used to separate the altitude of the longitude (for example / on the above lines) and the symbol used to separate the longitude from the course/speed (for example ' on the above lies)

-- "/z",  //  0 = ?
-- "/'",  //  1 = (moto-)glider    (most frequent)
-- "/'",  //  2 = tow plane        (often)
-- "/X",  //  3 = helicopter       (often)
-- "/g" , //  4 = parachute        (rare but seen - often mixed with drop plane)
-- "\\^", //  5 = drop plane       (seen)
-- "/g" , //  6 = hang-glider      (rare but seen)
-- "/g" , //  7 = para-glider      (rare but seen)
-- "\\^", //  8 = powered aircraft (often)
-- "/^",  //  9 = jet aircraft     (rare but seen)
-- "/z",  //  A = UFO              (people set for fun)
-- "/O",  //  B = balloon          (seen once)
-- "/O",  //  C = airship          (seen once)
-- "/'",  //  D = UAV              (drones, can become very common)
-- "/z",  //  E = ground support   (ground vehicles at airfields)
-- "\\n"  //  F = static object    (ground relay ?)

matchSymbol :: (Char, Char) -> AprsSymbol
matchSymbol =
  \case
    ('/', '\'') -> Glider
    ('/', 'X') -> Helicopter
    ('/', 'g') -> Parachute
    ('\\', '^') -> SmallPlane
    ('/', '^') -> LargePlane
    ('/', 'O') -> Baloon
    ('/', 'z') -> GroundVehicle
    ('\\', 'n') -> StaticObject
    _ -> Unknown
