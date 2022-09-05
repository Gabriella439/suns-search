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

{-| Utilities for mapping points to pages

    A page is spatial cube that I use to partition PDB structures.  One of the
    most important optimizations to the search algorithm is the requirement that
    search results must fit exactly within one of these pages.

    These pages also serve as useful search result chunks to return.  If you
    return the original structure you waste a lot of bandwidth and CPU power
    for the molecular visualization client.  If you return just the matching
    page you speed up the result delivery significantly, which is very often a
    bottle-neck on slower computers.
-}

module Page
    ( -- * Types
      Page
    , Size

      -- * Find bounding pages
    , pointToPage
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word64)
import Point (Point(Point))

-- | Each page is given a unique 64-bit ID
type Page = Word64

{-| The indexing program can change the size of pages to tune the tradeoff
    between search power and efficiency.  Larger pages permit more powerful
    search queries but slow down search speed.

    A 'Size' value of 0 corresponds to the smallest permissible page size:
    7.6 Å.  Each increase of 'Size' by 1 corresponds to a doubling of page size.
    Suns currently defaults to @Size = 1@, which is a page size of about 15.1 Å.
-}
type Size = Int

bitsPerValue :: Int
bitsPerValue = 21 -- The number of bits available to encode each dimension

{-# INLINE unpack #-}
unpack :: Word64 -> Word64
unpack x0 = let x1 =  x0                   .&. 0x00000000001FFFFF
                x2 = (x1 .|. shiftL x1 32) .&. 0x001F00000000FFFF
                x3 = (x2 .|. shiftL x2 16) .&. 0x001F0000FF0000FF
                x4 = (x3 .|. shiftL x3  8) .&. 0x100F00F00F00F00F
                x5 = (x4 .|. shiftL x4  4) .&. 0x10C30C30C30C30C3
             in      (x5 .|. shiftL x5  2) .&. 0x1249249249249249

zip3x21 :: Word64 -> Word64 -> Word64 -> Word64
zip3x21 x y z = unpack x .|. shiftL (unpack y) 1 .|. shiftL (unpack z) 2

-- Shrink page edges if the desired size is not near a power of 2
fudge :: Double
fudge = sqrt 2

pointToN :: Point -> Word64
pointToN (Point x y z) = zip3x21 (toInt x) (toInt y) (toInt z)
  where minV = -999.999
        maxV = 9999.999
        toInt v = truncate $ ((2^bitsPerValue - 1) * (v - minV) / (maxV - minV))
                            / fudge

-- Bits truncated per dimension; 10.7 Angstroms minimum without "fudge"
minTruncate :: Int
minTruncate = 11

-- | Convert a 'Point' to its containing 'Page', using the specified page 'Size'
pointToPage :: Size -> Point -> Page
pointToPage s p = shiftR (pointToN p) ((minTruncate + s) * 3)
