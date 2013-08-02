{-# LANGUAGE OverloadedStrings #-}

module Request (Request(..)) where

import Atom (Atom)
import Control.Monad (mzero)
import Data.Aeson (FromJSON(parseJSON), (.:), Value(Object))
import PDB (pdbToAtoms)
import Shuffle (Seed)

data Request = Request
    { rmsd      :: Double
    , numStruct :: Int
    , seed      :: Maybe Seed
    , atoms     :: [Atom]
    } deriving (Show)

instance FromJSON Request where
    parseJSON (Object v) = do
        _rmsd      <- v .: "rmsd_cutoff"
        _numStruct <- v .: "num_structures"
        nSeed      <- v .: "random_seed"
        let _seed = case nSeed of
                0 -> Nothing
                _ -> Just nSeed
        pdbStr     <- v .: "atoms"
        let _atoms = pdbToAtoms pdbStr
        return $ Request _rmsd _numStruct _seed _atoms
    parseJSON _ = mzero
