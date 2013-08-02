module Secondary (secondaryIndex, querySecondary) where

import Atom (Atom, atomToPage)
import Chemistry (ParseS)
import Control.Arrow (second, (&&&))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Correspond (match)
import qualified Data.ByteString as B
import Control.DeepSeq (NFData)
import Data.List (isSuffixOf, nub, sort)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Meld (meld)
import Motif (MotifGraphs, motifsInStructure)
import Page (Size)
import PDB (PDBID, pdbToAtoms)
import Pipes
import Primary (PrimaryIndex, queryPrimary)
import Proxy (foldVector, runVector, progress)
import Shuffle (Seed, shuffle)
import Structure (atomsToStructure)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>))
import Util (groupOn)

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

secondaryIndex
 :: Size
 -> FilePath
 -> MotifGraphs
 -> IO (V.Vector (PDBID, (VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))))
secondaryIndex size pdbDir parsers = do
    v <- runVector $ run $
         hoist lift (   filesToPageServer size pdbDir
                    >-> progress
                    >-> (for cat (yield . second (tokenize parsers)))
                    )
                    >-> foldVector
    putStrLn ""
    return v

tokenize
   :: MotifGraphs
   -> [Atom]
   -> (VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
tokenize parsers
  = (id &&& motifsInStructure parsers . atomsToStructure) . VS.fromList

querySecondary
    :: (NFData pdbID)
    => MotifGraphs
    -> PrimaryIndex
    -> V.Vector (pdbID, VS.Vector Atom, V.Vector (V.Vector (VS.Vector Int)))
    -> Maybe Seed
    -> [Atom]
    -> ([Atom], [(pdbID, [Atom], [Atom])])
querySecondary parsers i1 i2 mSeed atoms = do
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
