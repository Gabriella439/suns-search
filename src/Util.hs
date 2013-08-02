module Util (groupOn) where

import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord (comparing)

factor :: (Ord a) => [(a, b)] -> [(a, [b])]
factor abs = case abs of
    []       -> []
    (k, _):_ -> let (abs1, abs2) = span (\a -> fst a == k) abs
                 in (k, map snd abs1):factor abs2

groupOn :: (Ord a) => (b -> a) -> [b] -> [[b]]
groupOn toOrd = map snd . factor . sortBy (comparing fst) . map (toOrd &&& id)
