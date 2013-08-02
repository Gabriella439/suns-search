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
