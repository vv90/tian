module GeoTiff.Utils where

import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Relude

unpackTileRow :: (Int, Int) -> Vector (Vector Word16) -> [Vector Word16]
unpackTileRow (tileWidth, tileHeight) tiles =
  [ Vector.slice (row * tileWidth) tileWidth (tiles ! index)
    | row <- [0 .. tileHeight - 1],
      index <- [0 .. Vector.length tiles - 1]
  ]
