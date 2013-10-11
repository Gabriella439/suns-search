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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timeout (
    Milliseconds, Timeout(..), tryIO, runTimeout, runTimeoutP) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Pipes.Lift (runMaybeP, runReaderP)
import Pipes (Proxy, hoist)
import System.CPUTime (getCPUTime)
import System.Timeout (timeout)

type Milliseconds = Integer
type Picoseconds  = Integer

newtype Timeout r = Timeout { unTimeout :: ReaderT Picoseconds (MaybeT IO) r }
    deriving (Functor, Applicative, Monad)

tryIO :: IO r -> Timeout r
tryIO io = do
    let liftIO     = Timeout . lift . lift
        liftMaybe  = Timeout . lift
        liftReader = Timeout
    now <- liftIO getCPUTime
    end <- liftReader ask
    let remainder = fromIntegral $ (end - now) `div` 10^6 :: Int
    if (remainder > 0)
        then do
            m <- liftIO $ timeout remainder io
            case m of
                Nothing -> liftMaybe mzero
                Just r  -> return r
        else liftMaybe mzero

runTimeout :: Milliseconds -> Timeout r -> IO (Maybe r)
runTimeout duration m = do
    now <- getCPUTime
    let end = now + duration * 10^9
    runMaybeT $ runReaderT (unTimeout m) end

runTimeoutP
    :: Milliseconds -> Proxy a' a b' b Timeout r -> Proxy a' a b' b IO (Maybe r)
runTimeoutP duration p = do
    now <- lift getCPUTime
    let end = now + duration * 10^9
    runMaybeP $ runReaderP end $ hoist unTimeout p
