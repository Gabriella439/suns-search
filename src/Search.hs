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
import Indices (PrimaryIndex, SecondaryIndex, querySecondary)
import Kabsch (RMSD, aligner)
import Meld(meld)
import Motif (MotifGraphs, motifsInStructure)
import PDB (PDBID)
import Pipes
import Pipes.Core (Producer')
import Request (Request(Request))
import Structure (atomsToStructure)
import Timeout (Timeout, Milliseconds, runTimeoutP, tryIO)

pureSearch
 :: MotifGraphs
 -> PrimaryIndex
 -> SecondaryIndex
 -> Request
 -> [(PDBID, [Atom])]
pureSearch parsers i1 i2 (Request rmsd nMax mSeed atoms)
  = map fst
  . take nMax
  . filter ((< rmsd) . snd)
  . aligner
  . querySecondary parsers i1 i2 mSeed
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
 :: MotifGraphs
 -> PrimaryIndex
 -> SecondaryIndex
 -> Maybe Milliseconds
 -> (key, Request)
 -> Producer (key, Response (PDBID, [Atom])) IO ()
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
