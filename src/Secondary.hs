module Secondary (secondaryIndex) where

import Atom (Atom, atomToPage)
import Chemistry (ParseS)
import Control.Arrow (second, (&&&))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as B
import Data.List (isSuffixOf)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Motif (MotifGraphs, motifsInStructure)
import Page (Size)
import PDB (PDBID, pdbToAtoms)
import Pipes
import Proxy (foldVector, runVector, progress)
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
    let index
          = (id &&& motifsInStructure parsers . atomsToStructure)
          . VS.fromList
    v <- runVector $ run $
         hoist lift ( filesToPageServer size pdbDir      >->
                      progress                           >->
                      (for cat (yield . second index)) ) >->
         foldVector
    putStrLn ""
    return v
