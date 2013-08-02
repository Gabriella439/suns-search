-- | Quick and dirty PDB parsing

{-# LANGUAGE OverloadedStrings #-}

module PDB
    ( -- * PDB Parsing
      PDBID
    , pdbToAtoms
    ) where

import Atom (Atom(Atom, element), Prefix, Suffix)
import AtomName (AtomName, bsToAtomName)
import Control.Error (assertZ, headMay, justZ, rights)
import qualified Data.Attoparsec.Char8 as P
import qualified Data.ByteString.Char8 as B
import Element (Element, bsToElem)
import Point (Point(Point))

-- | 4 letter PDB code
type PDBID = String

decimal' = P.skipSpace >> P.decimal
double'  = P.skipSpace >> P.double

pPass1 :: P.Parser (AtomName, Element)
pPass1 = do
    record   <- P.take 6
    assertZ (record == "ATOM  " || record == "HETATM")
    _        <- P.take 6
    name     <- P.take 4
    altLoc   <- P.anyChar
    assertZ (elem altLoc " A")
    resName  <- P.take 3
    atomName <- justZ $ bsToAtomName (B.append resName name)
    _        <- P.take 56
    element' <- P.take 2
    element  <- justZ $ bsToElem element'
    return (atomName, element)

pPass2 :: P.Parser (Point, Prefix, Suffix)
pPass2 = do
    prefix <- P.take 30
    x      <- double'
    y      <- double'
    z      <- double'
    suffix <- P.take 26
    return (Point x y z, prefix, suffix)

parseAtom :: B.ByteString -> Either String Atom
parseAtom str = do
    (atomName, element)     <- P.parseOnly pPass1 str
    (point, prefix, suffix) <- P.parseOnly pPass2 str
    return $ Atom atomName point element prefix suffix

-- | Convert a 'B.ByteString' representation of a PDB file to a list of 'Atom's
pdbToAtoms :: B.ByteString -> [Atom]
pdbToAtoms
  = filter ((/= 0) . element) -- No hydrogens
  . rights
  . map parseAtom
  . takeWhile (not . B.isPrefixOf "ENDMDL")
  . B.lines
