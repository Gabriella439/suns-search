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

module Proxy (foldVector, runVector, progress) where

import Control.Monad (forever)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import Control.DeepSeq (NFData, ($!!))
import Control.Monad.ST.Safe (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Pipes
import System.IO (hFlush, stdout)

foldVector
   :: (NFData a) => Consumer a (StateT (Int, Int, VM.MVector RealWorld a) IO) r
foldVector = forever $ do
        (nElem, size, mv) <- liftState get
        if (nElem >= size)
            then do
                mv' <- liftIO $ VM.grow mv size
                liftState $ put (nElem, 2 * size, mv')
            else do
                a <- await
                liftIO $ VM.write mv nElem $!! a
                liftState $ put (nElem + 1, size, mv)
  where
    liftState = lift

runVector :: StateT (Int, Int, VM.MVector RealWorld a) IO r -> IO (V.Vector a)
runVector sio = do
    mv <- VM.new 1
    (nElem, _size, mv') <- execStateT sio (0, 1, mv)
    V.freeze (VM.slice 0 nElem mv')

progress :: Pipe a a IO r
progress = go 0 (0 :: Int)
  where
    go len n = do
        a <- await
        let str = show n 
        lift $ do
            putStr $ replicate len '\b'
            putStr str
            hFlush stdout
        yield a
        go (length str) (n + 1)
