{- Copyright 2013 Gabriella Gonzalez

   This file is part of the Suns Search Engine

   The Suns Search Engine is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or (at your
   option) any later version.

   The Suns Search Engine is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   the Suns Search Engine.  If not, see <http://www.gnu.org/licenses/>.
-}

module Util (groupOn) where

import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord (comparing)

factor :: (Ord a) => [(a, b)] -> [(a, [b])]
factor abs_ = case abs_ of
    []       -> []
    (k, _):_ -> let (abs1, abs2) = span (\a -> fst a == k) abs_
                 in (k, map snd abs1):factor abs2

groupOn :: (Ord a) => (b -> a) -> [b] -> [[b]]
groupOn toOrd = map snd . factor . sortBy (comparing fst) . map (toOrd &&& id)
