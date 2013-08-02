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
