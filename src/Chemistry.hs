-- | Substructure matching code

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chemistry
    ( -- * Type
      ParseS(..)
    , evalParseS

      -- * Substructure matching
    , pMotif
    ) where

import AtomName (AtomName)
import Control.Applicative (Applicative, Alternative)
import Control.Error (justZ)
import Control.Monad (MonadPlus, forM_, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (
    StateT(StateT), evalStateT, get, put )
import Data.Array ((!))
import qualified Data.Traversable as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Structure (Structure(Structure), bonds, deleteBond)

-- | A structure parser
newtype ParseS a = ParseS { unParseS :: StateT Structure [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

{-| Run a structure parser on a 'Structure' graph, returning a list of
    alternative solutions
-}
evalParseS :: ParseS a -> Structure -> [a]
evalParseS p = evalStateT (unParseS p)

pBond :: AtomName -> AtomName -> ParseS (Int, Int)
pBond name1 name2 = ParseS $ StateT $ \(Structure gr as) -> do
    i1 <- VS.toList $ VS.findIndices (== name1) as
    i2 <- filter (\i -> as VS.! i == name2) (gr ! i1)
    let newGraph = deleteBond (i1, i2) gr
    return ((i1, i2), Structure newGraph as)

{-| Match a motif from the current graph and remove it from the graph

    'pMotif' returns a 'VS.Vector' of matched vertices from graph the in the
    same order as the atoms from the parsed motif.
-}
pMotif :: Structure -> ParseS (VS.Vector Int)
pMotif (Structure bs as)
  = (`evalStateT` (V.replicate (VS.length as) Nothing)) $ do
        forM_ (bonds bs) $ \(i1, i2) -> do
            (i1', i2') <- lift $ pBond (as VS.! i1) (as VS.! i2)
            matches    <- get
            let consistent i1 i1' = case (matches V.! i1) of
                    Nothing   -> True
                    Just iOld -> iOld == i1'
            guard $ consistent i1 i1' && consistent i2 i2'
            put $ matches V.// [(i1, Just i1'), (i2, Just i2')]
        matchesFinal <- get
        fmap G.convert $ justZ $ T.sequence matchesFinal
