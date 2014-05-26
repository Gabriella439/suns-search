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

{-| High-level utilities for reading/writing 'MotifGraphs' and querying
    'Structure's with them
-}

module Motif
    ( -- * Types
      MotifGraphs
    , numMotifs

      -- * Read in motifs
    , motifsFromDir

      -- * Search using motifs
    , motifsInStructure
    ) where

import Chemistry (evalParseS, pMotif)
import Control.Applicative (many)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (filterM, forM, msum)
import qualified Data.ByteString as B
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import HSerialize (HSerialize(get, put))
import PDB (pdbToAtoms)
import Structure (Structure, atomsToStructure)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath.Posix ((</>))

newtype MotifGraphs = MotifGraphs { unMotifGraphs :: [[Structure]] }

instance NFData MotifGraphs where
    rnf (MotifGraphs m) = rnf m

instance HSerialize MotifGraphs where
    get = fmap MotifGraphs get
    put = put . unMotifGraphs

-- | Query the number of motifs
numMotifs :: MotifGraphs -> Int
numMotifs = length . unMotifGraphs

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs dir = do
    files <- getDirectoryContents dir
    dirs <- filterM (doesDirectoryExist . (dir </>)) $ files
    return $ filter (not . isPrefixOf ".") dirs

{-| Preprocess the contents of a motif directory into a format suitable for
    searching and return a serializable 'MotifGraphs'.
-}
motifsFromDir :: FilePath -> IO MotifGraphs
motifsFromDir dir = fmap MotifGraphs $ do
    motifDirs <- getSubDirs dir
    forM motifDirs $ \motifDir -> do
        files <- getDirectoryContents (dir </> motifDir)
        let motifFiles = filter (isSuffixOf ".pdb") files
        forM motifFiles $ \motifFile -> do
            str <- B.readFile $ dir </> motifDir </> motifFile
            return . atomsToStructure . VS.fromList . pdbToAtoms $ str

{-| Given 'MotifGraphs' and a 'Structure', find as many matches to all motifs,
    allowing multiple matches of each motif, but forbidding overlapping bonds.

    The outer 'V.Vector' of the result has one element per motif, and the inner
    'V.Vector' stores the return values of all of the successful matches of that
    motif.  Suns only uses 'ParseS' where the @match@ return value has type
    @Vector Int@, representing a list of matched atom indices, so the result
    type will end up being @Vector (Vector (Vector Int))@.  The reason I don't
    specialize the type and keep @match@ polymorphic is to improve the clarity
    of the type signature and to use parametricity to guard against potential
    errors.
-}
motifsInStructure
 :: MotifGraphs
 -> Structure
 -> V.Vector (V.Vector (VS.Vector Int))
motifsInStructure (MotifGraphs motifs) structure
  = let 
        motifsParser =
            fmap V.fromList
          . mapM (fmap V.fromList . many . msum . map pMotif)
          $ motifs
        -- This head is safe, because "many" always returns at least one result
        -- Unfortunately, I don't know how to easily encode that in the types
     in head $ evalParseS motifsParser structure
