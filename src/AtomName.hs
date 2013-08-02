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
