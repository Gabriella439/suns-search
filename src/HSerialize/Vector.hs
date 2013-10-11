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

-- | 'HSerialize' instances for boxed and storable 'Vector's

{-# LANGUAGE ScopedTypeVariables #-}

module HSerialize.Vector where

import HSerialize.Core

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import System.IO (Handle, hGetBuf, hPutBuf)
import Foreign.Safe as F

instance (Storable e) => HSerialize (VS.Vector e) where
    put v = do
        let nelem = VS.length v
            elsize = F.sizeOf (undefined :: e)
        put nelem
        ReaderT $ \h -> VS.unsafeWith v $ \p -> hPutBuf h p (nelem * elsize)
    get = do
        nelem <- get
        let elsize = F.sizeOf (undefined :: e)
        ReaderT $ \h -> do
            fp <- F.mallocForeignPtrArray nelem
            F.withForeignPtr fp $ \p -> hGetBuf h p (nelem * elsize)
            return $ VS.unsafeFromForeignPtr0 fp nelem

instance (HSerialize a) => HSerialize (V.Vector a) where
    put v = do
        put (V.length v)
        mapM_ put (V.toList v)
    get = do
        n  <- get
        mv <- lift $ VM.new n
        let fill i
                | i < n = do
                    x <- get
                    lift $ VM.unsafeWrite mv i x
                    fill (i + 1)
                | otherwise = return ()
        fill 0
        lift $ V.unsafeFreeze mv
