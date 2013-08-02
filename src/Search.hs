module Search (Response(..), search) where

import Atom (Atom)
import Control.Arrow ((&&&))
import Control.DeepSeq (NFData, force)
import Control.Exception.Base (evaluate)
import Control.Error (headMay)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Correspond (match)
import Data.List (nub, sort)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Kabsch (RMSD, aligner)
import Meld(meld)
import Motif (MotifGraphs, motifsInStructure)
import Pipes
import Pipes.Core (Producer')
import Primary (queryPrimary, PrimaryIndex)
import Request (Request(Request))
import Shuffle (Seed, shuffle)
import Structure (atomsToStructure)
import Timeout (Timeout, Milliseconds, runTimeoutP, tryIO)

{- This duplicates some code from Secondary.hs because I might later want to
   independently change how the query or secondary index behave.  The fact that
   they currently behave identically is just a coincidence. -}
queryToIndex
   :: MotifGraphs
   -> [Atom]
   -> (VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
queryToIndex parsers
  = (id &&& motifsInStructure parsers . atomsToStructure) . VS.fromList

queryPages
 :: (NFData pdbID)
 => MotifGraphs
 -> PrimaryIndex
 -> V.Vector (pdbID, VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
 -> Maybe Seed
 -> [Atom]
 -> ([Atom], [(pdbID, [Atom], [Atom])])
queryPages parsers i1 i2 mSeed atoms = do
    let (queryAtoms, queryMotifs) = queryToIndex parsers (meld atoms)
        querySubset = map (queryAtoms VS.!) . sort . nub $ do
            motif     <- V.toList queryMotifs
            incidence <- V.toList motif
            VS.toList incidence
        candidates = case (queryPrimary i1 queryMotifs) of
            Nothing    -> []
            Just pages -> do
                page <-
                    (case mSeed of
                        Nothing   -> id
                        Just seed -> shuffle seed (S.size pages)
                    ) (S.toList pages)
                let (pdbID, pageAtoms, pageMotifs) = i2 V.! page
                    context = VS.toList pageAtoms
                map (\indices -> (pdbID, map (pageAtoms VS.!) indices, context))
                  $ match queryMotifs pageMotifs
     in (querySubset, candidates)

pureSearch
 :: (NFData pdbID)
 => MotifGraphs
 -> PrimaryIndex
 -> V.Vector (pdbID, VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
 -> Request
 -> [(pdbID, [Atom])]
pureSearch parsers i1 i2 (Request rmsd nMax mSeed atoms)
  = map fst
  . take nMax
  . filter ((< rmsd) . snd)
  . aligner
  . queryPages parsers i1 i2 mSeed
  $ atoms

data Response a = Result a | Done | Timeout | Error String

timedList :: (NFData a) => [a] -> Producer' a Timeout ()
timedList = go where
    go as = do
        h <- lift $ tryIO $ evaluate $ force $ headMay as
        case h of
            Nothing -> return ()
            Just a  -> do
                yield a
                go (tail as)

search
 :: (NFData pdbID)
 => MotifGraphs
 -> PrimaryIndex
 -> V.Vector (pdbID, VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
 -> Maybe Milliseconds
 -> (key, Request)
 -> Producer (key, Response (pdbID, [Atom])) IO ()
search parsers i1 i2 timeout (key, req) = do
    let results = pureSearch parsers i1 i2 req
        answer  = do
            success <- case timeout of
                Nothing -> fmap Just $
                    for (each results) (yield . Result)
                Just milliseconds -> runTimeoutP milliseconds $
                    for (timedList results) (yield . Result)
            yield $ case success of
                Just () -> Done
                Nothing -> Timeout
    for answer (yield . ((,) key))
