module RwLenUnits where

import Relude

data RwLenUnits
  = Meters
  | NauticalMiles
  | StatuteMiles
  deriving (Eq, Show, Read)
