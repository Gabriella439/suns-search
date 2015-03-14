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

{-# LANGUAGE OverloadedStrings #-}

module Request (Request(..)) where

import Atom (Atom)
import Control.Monad (mzero)
import Data.Aeson (FromJSON(parseJSON), (.:), Value(Object))
import Data.Text.Encoding (encodeUtf8)
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
        pdbStr     <- fmap encodeUtf8 (v .: "atoms")
        let _atoms = pdbToAtoms pdbStr
        return $ Request _rmsd _numStruct _seed _atoms
    parseJSON _ = mzero
