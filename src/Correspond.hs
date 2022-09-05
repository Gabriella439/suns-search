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

    -- For each motif type (for both the query and index)
    forM_ (V.toList (V.zip query index)) $ \(qMotif, iMotif) -> do

        -- For each match to the motif type (in the query)
        forM_ (V.toList qMotif) $ \qIncidence -> do

            -- Select one match to the same motif type (from the index)
            --
            -- What may not be obvious to people unfamiliar with the
            -- `StateT s []` monad is that this command causes the computation
            -- to branch and each branch maintains its own independent state
            -- that is totally isolated from the state of other branches.
            iIncidence <- lift $ V.toList iMotif

            -- The following segment uses a "bimap" (i.e. a two-way map,
            -- implemented as a pair of maps) to keep track of correspondences
            -- between atoms in the index and the query.
            --
            -- Every time we match up a motif in the query with a motif in the
            -- index we:
            --
            -- A) record correspondences between their atoms
            -- B) ensure that these new correspondences are consistent with
            --    previously established correspondences
            --
            -- Note that an index atom may match multiple query atoms because
            -- we allow the query to contain duplicated atoms.  So we only
            -- verify that:
            -- 
            -- A) each query atom matches EXACTLY one index atom
            -- B) each index atom matches AT LEAST one query atom
            let l1 = VS.toList qIncidence
                l2 = VS.toList iIncidence
            forM_ (zip l1 l2) $ \(qIx, iIx) -> do
                (qMap, iMap) <- get
                case (M.lookup qIx qMap, M.lookup iIx iMap) of
                    -- No previous matches, so no conflict
                    (Nothing, Nothing) -> put
                        ( M.insert qIx iIx qMap
                        , M.insert iIx qIx iMap
                        )
                    -- Previous matches, so check for a conflict
                    (Just iIx', Just qIx')
                        -- They are the same match, so we're all good
                        | qIx == qIx' && iIx == iIx' -> return ()
                        -- They don't match, bail out of this solution
                        | otherwise                  -> mzero
                    -- One way match.  This is possible and valid because of
                    -- duplicate query atoms.
                    (Nothing, Just _qIx') -> put
                        ( M.insert qIx iIx qMap
                        , iMap
                        )
                    _ -> mzero

    gets (map snd . sortBy (comparing fst) . M.assocs . fst)
