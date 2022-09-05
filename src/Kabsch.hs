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

-- | Alignment of structures using the Kabsch alignment algorithm

module Kabsch (
      -- * Type
      RMSD

      -- * Alignment
    , aligner
    ) where

import Atom (Atom(point), distSqA)
import Data.List (tails)
import Data.Maybe (catMaybes)
import Point (pointToList, listToPoint)
import qualified Numeric.LinearAlgebra as N
import Numeric.LinearAlgebra ((<>))

-- | Root-mean-square-deviation between two structures, in Ångstroms
type RMSD = Double

type Alignment = (N.Matrix Double, N.Matrix Double, N.Matrix Double)

toMatrix :: [Atom] -> N.Matrix Double
toMatrix = N.fromLists . map (pointToList . point)

fromMatrix :: [Atom] -> N.Matrix Double -> [Atom]
fromMatrix as m = zipWith
    (\a p -> a { point = p })
    as
    (catMaybes . map listToPoint . N.toLists $ m)

mean :: N.Matrix Double -> N.Matrix Double
mean m = let n = N.rows m in N.scale (1 / fromIntegral n) $ N.row (replicate n 1) <> m

(-:) :: N.Matrix Double -> N.Matrix Double -> N.Matrix Double
m -: mc = m - N.repmat mc (N.rows m) 1

(+:) :: N.Matrix Double -> N.Matrix Double -> N.Matrix Double
m +: mc = m + N.repmat mc (N.rows m) 1

-- Arguments are Nx3 matrices where each row is a coordinate
params :: N.Matrix Double -> N.Matrix Double -> Alignment
params m2 m1 = -- Aligns m1 to m2
    let m1c = mean m1
        m2c = mean m2
        m1' = m1 -: m1c
        m2' = m2 -: m2c
        a = N.tr' m1' <> m2'
        (u, _s, v) = N.svd a
        d = signum (N.det $ v <> N.tr' u)
        i = N.fromLists [[1, 0, 0], [0, 1, 0], [0, 0, d]]
        r = v <> i <> N.tr' u
     in (m1c, m2c, r)

align :: Alignment -> N.Matrix Double -> N.Matrix Double
align (m1c, m2c, r) m1 = ((m1 -: m1c) <> N.tr' r) +: m2c

{-| Aligns a list of structures, represented as @(pdbID, substructure, full)@,
    where:

    * @pdbID@: A placeholder for the structure's PDB ID

    * @substructure@: The subset of the structure to align on

    * @full@: The full structure to return, aligned along the given substructure

    The first argument of the tuple is an arbitrarily chosen substructure that
    every other substructure will align to.  Just use the first substructure.

    All substructures must have the same number of atoms.  This precondition is
    not checked.

    The result is the aligned full structures, paired with their PDB ID and the
    RMSD of the alignment.

    The types and tuples are chosen to simplify interfacing with the internal
    code in the "Search" module.
-}
aligner :: ([Atom], [(pdbID, [Atom], [Atom])]) -> [((pdbID, [Atom]), RMSD)]
aligner (query, candidates)
  = let cutoff = 1.0 -- Angstroms
        cutoffSq = cutoff * cutoff
        keep =
            zipWith
                (\a1 rest -> all (\a2 -> distSqA a1 a2 > cutoffSq) rest)
                query
                (tail $ tails query)
        unique = map snd . filter fst . zip keep
        mq = toMatrix (unique query)
     in flip map candidates $ \(pdbID, subset, context) ->
            let mrs  = toMatrix (unique subset)
                mrc  = toMatrix context
                ps   = params mq mrs
                mrs' = align ps mrs
                mrc' = align ps mrc
                rmsd = sqrt $
                        N.sumElements (N.cmap (^(2::Int)) $ mrs' - mq)
                    /   fromIntegral (N.rows mrs')
             in ((pdbID, fromMatrix context mrc'), rmsd)
