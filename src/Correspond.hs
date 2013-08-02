-- | Substructure search

module Correspond (
      -- * Type
      Tokenized

      -- * Search
    , match
    ) where

import Control.Monad (mzero, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT, get, gets, put)
import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

{-| Data structure that stores all matches of all motifs to a given structure

> +-- Motif (i.e. the first element is all matches to the first motif)
> |
> |       +-- Match number (i.e. the first element is the first match)
> |       |
> |       |       +-- Indices of atoms matched (i.e. the first element is the
> |       |       |   index that matched the first atom in the motif)
> v       v       v
> Vector (Vector (Vector Int))
-}
type Tokenized = V.Vector (V.Vector (VS.Vector Int))

{-| This is the core searching logic

    'match' takes two 'Tokenized' structures, the first of which is the search
    query and the second of which is the target structure to search.

    'match' returns a list of solutions.  Each solution is a list of 'Int's,
    with one element per atom in the query.  The 'Int's stored in each solution
    represent the indices of the atoms matched in the structure, in the order
    of the original query atoms.

    This means that the first element in a solution will be the index of the
    atom in the target structure that matches the first query atom.
-}
match :: Tokenized -> Tokenized -> [[Int]]
match query index = (`evalStateT` (M.empty, M.empty)) $ do
    forM_ (V.toList (V.zip query index)) $ \(qMotif, iMotif) -> do
        forM_ (V.toList qMotif) $ \qIncidence -> do
            iIncidence <- lift $ V.toList iMotif
            let l1 = VS.toList qIncidence
                l2 = VS.toList iIncidence
            forM_ (zip l1 l2) $ \(qIx, iIx) -> do
                (qMap, iMap) <- get
                case (M.lookup qIx qMap, M.lookup iIx iMap) of
                    (Nothing, Nothing) -> put (
                        M.insert qIx iIx qMap,
                        M.insert iIx qIx iMap)
                    (Just iIx', Just qIx')
                        | qIx == qIx' && iIx == iIx' -> return ()
                        | otherwise -> mzero
                    (Nothing, Just qIx') -> put (M.insert qIx iIx qMap, iMap)
                    _ -> mzero
    gets (map snd . sortBy (comparing fst) . M.assocs . fst)
