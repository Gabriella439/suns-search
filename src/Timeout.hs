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
