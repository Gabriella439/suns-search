{-# LANGUAGE ScopedTypeVariables #-}

{-| Fast serialization and deserialization specialized to 'Handle's.  The \'H\'
    in 'HSerialize' stands for 'Handle'
-}

module HSerialize.Core (
      -- * The HSerialize Class
      HSerialize(get, put)

      -- * Encoding and Decoding
    , encodeFile
    , decodeFile

      -- * Default Storable Instance
    , Store(Store, unStore)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Data.Array (Array, Ix, listArray, bounds, elems)
import System.IO as IO
import Foreign.Safe as F

class HSerialize t where
    get :: ReaderT IO.Handle IO t
    put :: t -> ReaderT IO.Handle IO ()

encodeFile :: (HSerialize t) => FilePath -> t -> IO ()
encodeFile file t = IO.withFile file IO.WriteMode $ runReaderT (put t)

decodeFile :: (HSerialize t) => FilePath -> IO t
decodeFile file = IO.withFile file ReadMode $ runReaderT get

newtype Store a = Store { unStore :: a }

instance (F.Storable a) => HSerialize (Store a) where
    put n = ReaderT $ \h -> F.with (unStore n) $ \p ->
        IO.hPutBuf h p (F.sizeOf (undefined :: a))
    get   = ReaderT $ \h -> alloca $ \p -> do
        IO.hGetBuf h p (F.sizeOf (undefined :: a))
        fmap Store $ F.peek p

instance HSerialize Char where
    put = put . Store
    get = fmap unStore get

instance HSerialize Word8 where
    put = put . Store
    get = fmap unStore get

instance HSerialize Int where
    put = put . Store
    get = fmap unStore get

instance (HSerialize a) => HSerialize [a] where
    put as = do
        if (null as)
            then put (0 :: Word8)
            else do
                put (1 :: Word8)
                let chunkSize = 100 :: Int
                    (prefix, suffix) = splitAt chunkSize as
                if (null suffix)
                    then put (length prefix)
                    else put chunkSize
                mapM_ put prefix
                put suffix
    get = do
        b <- get :: ReaderT IO.Handle IO Word8
        case b of
            0 -> return []
            1 -> do
                n <- get :: ReaderT IO.Handle IO Int
                prefix <- replicateM n get
                fmap (prefix ++) get
            _ -> lift
               $ throwIO
               $ userError "HSerialize [a]: get - Invalid format"

instance (HSerialize a, HSerialize b) => HSerialize (a, b) where
    put (a, b) = do
        put a
        put b
    get = (,) <$> get <*> get

instance (HSerialize a, HSerialize b, HSerialize c)
 => HSerialize (a, b, c) where
    put (a, b, c) = do
        put a
        put b
        put c
    get = (,,) <$> get <*> get <*> get

instance (Ix i, HSerialize i, HSerialize e) => HSerialize (Array i e) where
    put arr = put (bounds arr, elems arr)
    get = fmap (uncurry listArray) get
