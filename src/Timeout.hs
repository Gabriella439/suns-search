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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Utilities for timed computations, used to limit search requests to a fixed
    amount of time
-}
module Timeout (
    -- * Timeouts
    Timeout,

    -- * Run timeouts
    Milliseconds,
    tryIO,
    runTimeout,
    runTimeoutP
    ) where

import Control.Applicative (Applicative)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Pipes.Lift (runMaybeP, runReaderP)
import Pipes (Proxy, hoist)
import System.CPUTime (getCPUTime)
import System.Timeout (timeout)

type Milliseconds = Integer
type Picoseconds  = Integer

{-| The 'Timeout' monad keeps track of the total elapsed time since it began,
    preventing computations from running past a final time point
-}
     
newtype Timeout r = Timeout { unTimeout :: ReaderT Picoseconds (MaybeT IO) r }
    deriving (Functor, Applicative, Monad)

{-| Try to run an 'IO' action

    If the current 'Timeout' monad has exceeded its total allotted time, the
    action will not run.  Otherwise the action will run, but will be constrained
    to run within the remaining allotted time and will be interrupted if it
    exceeds that time.
-}
tryIO :: IO r -> Timeout r
tryIO io = do
    let liftIO     = Timeout . lift . lift
        liftMaybe  = Timeout . lift
        liftReader = Timeout
    now <- liftIO getCPUTime
    end <- liftReader ask
    let remainder = fromIntegral $ (end - now) `div` 10^(6::Int) :: Int
    if (remainder > 0)
        then do
            m <- liftIO $ timeout remainder io
            case m of
                Nothing -> liftMaybe mzero
                Just r  -> return r
        else liftMaybe mzero

{-| Run a 'Timeout' monad, specifying the maximum time the timed computation may
    use, interrupting the computation if it exceeds that allotted time.

    Returns 'Nothing' if the computation is interrupted, and returns 'Just' if
    the computation completes successfully within the allotted time
-}
runTimeout :: Milliseconds -> Timeout r -> IO (Maybe r)
runTimeout duration m = do
    now <- getCPUTime
    let end = now + duration * 10^(9::Int)
    runMaybeT $ runReaderT (unTimeout m) end

-- | Unwrap a 'Timeout' monad buried within a pipe
runTimeoutP
    :: Milliseconds -> Proxy a' a b' b Timeout r -> Proxy a' a b' b IO (Maybe r)
runTimeoutP duration p = do
    now <- lift getCPUTime
    let end = now + duration * 10^(9::Int)
    runMaybeP $ runReaderP end $ hoist unTimeout p
