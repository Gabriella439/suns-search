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

{-| Atom names are encoded from seven-character ASCII strings built from
    concatenating:

    * a 4-character PDB atom name that uniquely identifies the atom within the
      residue

    * a 3-character residue name
-}

module AtomName
    ( -- * Type
      AtomName

      -- * Conversion routines
    , bsToAtomName
    , nameToBs) where

import qualified Data.ByteString as B
import Foreign.Safe (Word64, alloca, copyBytes, castPtr, peek, with)
import Foreign.Marshal.Unsafe (unsafeLocalState)

-- | An atom name is encoded as 64-bit int for compactness
type AtomName = Word64

{-| Encode a 'B.ByteString' to an 'AtomName', returning 'Nothing' if encoding
    failed
-}
bsToAtomName :: B.ByteString -> Maybe AtomName
bsToAtomName bs
  = if (B.length bs == 7)
    then Just $ unsafeLocalState $ alloca $ \pWord64 ->
        B.useAsCString bs $ \pByteString -> do
            copyBytes pWord64 (castPtr pByteString) 8
            peek pWord64
    else Nothing

-- | Decode an 'B.ByteString' from an 'AtomName'
nameToBs :: AtomName -> B.ByteString
nameToBs name = unsafeLocalState $ with name $ \pWord64 ->
    B.packCString (castPtr pWord64)
