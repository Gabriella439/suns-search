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

-- | Core logic for creating and querying the search indices

module Indices
    (
      -- * Primary Index
      PrimaryIndex
    , primaryIndex
    , queryPrimary

      -- * Secondary Index
    , secondaryIndex
    , SecondaryIndex
    , querySecondary
    ) where

import Atom (Atom, atomToPage)
import Chemistry (ParseS)
import Control.Arrow (second, (&&&))
import Control.Error (maximumDef)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Correspond (match)
import qualified Data.ByteString as B
import Control.DeepSeq (NFData(rnf))
import Data.List (isSuffixOf, nub, sort)
import Data.Monoid (Monoid(mempty))
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import HSerialize(HSerialize(get, put))
import Meld (meld)
import Motif (MotifGraphs, motifsInStructure)
import Page (Size)
import PDB (PDBID, pdbToAtoms)
import Pipes
import Proxy (foldVector, runVector, progress)
import Shuffle (Seed, shuffle)
import Structure (atomsToStructure)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>))
import Util (groupOn)

{-| The forward index, used to rapidly eliminate structural 'Page's before
    progressing to the more detailed checks in the secondary index
-}
newtype PrimaryIndex = PrimaryIndex
    { unPrimaryIndex :: V.Vector (V.Vector (S.Set Int)) }

instance NFData PrimaryIndex where
    rnf (PrimaryIndex i) = rnf i

instance HSerialize PrimaryIndex where
    get = fmap PrimaryIndex get
    put = put . unPrimaryIndex

{-| Given a 'V.Vector' of tokenized structures, return the primary index.  The
    'Int' parameter is the number of motifs, which I pass as a programming
    convenience to avoid having to extract this number from the first entry.
-}
primaryIndex
 :: Int
 -> SecondaryIndex
 -> PrimaryIndex
primaryIndex numMotif
  = PrimaryIndex
  . V.map (V.drop 1 . countVector)
  . V.foldr (V.zipWith (++)) (V.replicate numMotif [])
  . V.imap (\i (_, _, v) -> V.map (\matches -> [(V.length matches, i)]) v)
  . unSecondaryIndex

countVector :: (Ord a) => [(Int, a)] -> V.Vector (S.Set a)
countVector
  = hashmapToVector
  . H.map S.fromList
  . H.fromListWith (++)
  . map (second return)

hashmapToVector :: (Monoid a) => H.HashMap Int a -> V.Vector a
hashmapToVector h =
    let nMax = maximumDef 0 $ H.keys h
        v = V.replicate (nMax + 1) mempty
     in v V.// H.toList h

{-| Query the 'PrimaryIndex' using a tokenized structure, returning a 'S.Set'
    of all pages that matched.

    Returns 'Nothing' if the tokenized structure was empty and therefore matched
    every page trivially.
-}
queryPrimary
 :: PrimaryIndex
 -> V.Vector (V.Vector match)
 -> Maybe (S.Set Int)
queryPrimary (PrimaryIndex i1) iq
  = V.foldl'
        (\acc elem -> case acc of
            Nothing -> elem
            Just s1 -> case elem of
                Nothing -> acc
                Just s2 -> Just $ S.intersection s1 s2 )
        Nothing
  $ V.zipWith
        (\incidences count ->
            if (count < 1)
            then Nothing
            else Just
               . V.foldl' S.union S.empty
               . V.drop (count - 1)
               $ incidences)
        i1
        (V.map V.length iq)

{-| The detailed index, which stores all details of the original structural
    'Page's and also caches the locations of every motif advertised in the
    forward index
-}
newtype SecondaryIndex = SecondaryIndex
    { unSecondaryIndex
        :: V.Vector (PDBID, VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
    }

instance NFData SecondaryIndex where
    rnf (SecondaryIndex i) = rnf i

instance HSerialize SecondaryIndex where
    get = fmap SecondaryIndex get
    put = put . unSecondaryIndex

filesToPageServer
 :: Size -> FilePath -> Producer (PDBID, [Atom]) IO ()
filesToPageServer size pdbDir = do
    files <- lift $ getDirectoryContents pdbDir
    let pdbFiles = filter (isSuffixOf ".pdb") files
    forM_ pdbFiles $ \pdbFile -> do
        str <- lift $ B.readFile (pdbDir </> pdbFile)
        let pdbID       = take 4 pdbFile
            allAtoms    = pdbToAtoms str
            atomsByPage = groupOn (atomToPage size) allAtoms
        forM_ atomsByPage $ \atoms -> yield (pdbID, atoms)

{-| Given a page 'Size' a PDB file directory, and a set of parsed motifs,
    compute the 'SecondaryIndex'
-}
secondaryIndex
 :: Size
 -> FilePath
 -> MotifGraphs
 -> IO SecondaryIndex
secondaryIndex size pdbDir parsers = do
    v <- runVector $ runEffect $
         hoist lift (   filesToPageServer size pdbDir
                    >-> progress
                    >-> (for cat (yield . f . second (tokenize parsers)))
                    )
                    >-> foldVector
    putStrLn ""
    return (SecondaryIndex v)
  where
    f (a, (b, c)) = (a, b, c)

tokenize
   :: MotifGraphs
   -> [Atom]
   -> (VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
tokenize parsers
  = (id &&& motifsInStructure parsers . atomsToStructure) . VS.fromList

querySecondary
    :: MotifGraphs
    -> PrimaryIndex
    -> SecondaryIndex
    -> Maybe Seed
    -> [Atom]
    -> ([Atom], [(PDBID, [Atom], [Atom])])
querySecondary parsers i1 (SecondaryIndex i2) mSeed atoms = do
    let (queryAtoms, queryMotifs) = tokenize parsers (meld atoms)
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
