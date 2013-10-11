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

module Shuffle (Seed, shuffle) where

import Control.Monad (forM_)
import Control.Monad.Random (mkStdGen, evalRandT, getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Seed = Int
 
shuffle :: Seed -> Int -> [Int] -> [Int]
shuffle seed n xs
  = let g  = mkStdGen seed
        v2 = VU.create $ do
            mv <- VU.thaw $ VU.fromListN n xs
            let n' = n - 1
            (`evalRandT` g) $ forM_ [0..n'] $ \i -> do
                j  <- getRandomR (i, n')
                lift $ do 
                    vi <- VUM.unsafeRead mv i
                    vj <- VUM.unsafeRead mv j
                    VUM.unsafeWrite mv i vj
                    VUM.unsafeWrite mv j vi
            return mv
     in VU.toList v2
