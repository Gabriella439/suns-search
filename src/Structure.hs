{- Copyright 2013 Gabriel Gonzalez

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

module Structure (Structure(..), atomsToStructure, bonds, deleteBond) where

import Atom (Atom(name), distSqA)
import AtomName (AtomName)
import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (guard)
import Data.Array ((//), (!))
import Data.Graph (Graph, Edge, buildG, edges)
import Data.List (delete, findIndices)
import qualified Data.Vector.Storable as VS
import HSerialize (HSerialize(get, put))

data Structure = Structure {
    graph :: Graph             ,
    atoms :: VS.Vector AtomName}
    deriving (Show)

instance HSerialize Structure where
    put (Structure g as) = do
        put g
        put as
    get = Structure <$> get <*> get

instance NFData Structure where
    rnf (Structure gr as) = rnf gr `seq` rnf as `seq` ()

cutoff, cutoffSq :: Double
cutoff   = 2 -- Angstroms
cutoffSq = cutoff * cutoff

atomsToEdges :: VS.Vector Atom -> [Edge]
atomsToEdges as = do
    let as' = VS.toList as
    (i1, a1) <- zip [0..] as'
    i2       <- findIndices (\a2 -> distSqA a1 a2 < cutoffSq) as'
    guard  (i1 /= i2)
    return (i1,   i2)

atomsToStructure :: VS.Vector Atom -> Structure
atomsToStructure as
  = let graph_ = buildG (0, VS.length as - 1) (atomsToEdges as)
        names = VS.map name as
     in Structure graph_ names

bonds :: Graph -> [Edge]
bonds = filter (uncurry (<=)) . edges

deleteBond :: Edge -> Graph -> Graph
deleteBond (i1, i2) g
  = let i1adj  = g ! i1
        i2adj  = g ! i2
        i1adj' = delete i2 i1adj
        i2adj' = delete i1 i2adj
     in g // [(i1, i1adj'), (i2, i2adj')]
