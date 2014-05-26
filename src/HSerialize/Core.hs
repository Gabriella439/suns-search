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

{-# LANGUAGE ScopedTypeVariables #-}

{-| Fast serialization and deserialization specialized to 'Handle's.  The \'H\'
    in 'HSerialize' stands for 'Handle'

    I do not use @binary@ or @cereal@ because they were both really slow the
    last time I checked.  So I wrote a serialization interface specialized to
    interacting with 'Handle's in 'IO'.  This improved performance 5-fold the
    last time I measured it, but those libraries may have improved since then si
    this solution may be obsolete.
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
        _ <- IO.hGetBuf h p (F.sizeOf (undefined :: a))
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
