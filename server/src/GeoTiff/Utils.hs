module GeoTiff.Utils where

import Relude
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

unpackTileRow :: (Int, Int) -> Vector (Vector Word16) -> [Vector Word16]
unpackTileRow (tileWidth, tileHeight) tiles = 
    [ Vector.slice (row * tileWidth) tileWidth (tiles ! index)
    | row <- [0 .. tileHeight - 1]
    , index <- [0 .. Vector.length tiles - 1]
    ]