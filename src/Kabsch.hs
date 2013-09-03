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
import qualified Matrix as N
import Matrix ((<>))
import Matrix (ones)
import Point (pointToList, listToPoint)

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

mean m = let n = N.rows m in N.scale (1 / fromIntegral n) $ ones 1 n <> m

m -: mc = m - N.repmat mc (N.rows m) 1
m +: mc = m + N.repmat mc (N.rows m) 1

-- Arguments are Nx3 matrices where each row is a coordinate
params :: N.Matrix Double -> N.Matrix Double -> Alignment
params m2 m1 = -- Aligns m1 to m2
    let m1c = mean m1
        m2c = mean m2
        m1' = m1 -: m1c
        m2' = m2 -: m2c
        a = N.trans m1' <> m2'
        (u, s, v) = N.svd a
        d = signum (N.det $ v <> N.trans u)
        i = N.fromLists [[1, 0, 0], [0, 1, 0], [0, 0, d]]
        r = v <> i <> N.trans u
     in (m1c, m2c, r)

align :: Alignment -> N.Matrix Double -> N.Matrix Double
align (m1c, m2c, r) m1 = ((m1 -: m1c) <> N.trans r) +: m2c

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
                rmsd = sqrt $   N.sumElements (N.mapMatrix (^2) $ mrs' - mq)
                              / fromIntegral (N.rows mrs')
             in ((pdbID, fromMatrix context mrc'), rmsd)
