module Utils where

import Relude

within :: (Num a, Show a, Ord a) => a -> a -> a -> Bool
within eps x y = abs (x - y) <= eps