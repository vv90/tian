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
