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
  = let graph = buildG (0, VS.length as - 1) (atomsToEdges as)
        names = VS.map name as
     in Structure graph names

bonds :: Graph -> [Edge]
bonds = filter (uncurry (<=)) . edges

deleteBond :: Edge -> Graph -> Graph
deleteBond (i1, i2) g
  = let i1adj  = g ! i1
        i2adj  = g ! i2
        i1adj' = delete i2 i1adj
        i2adj' = delete i1 i2adj
     in g // [(i1, i1adj'), (i2, i2adj')]
