{-| Internal representation of an 'Atom'

    The 'Atom' type remembers the original 'B.ByteString' sans coordinates so
    that it can rapidly generate output coordinate files aligned to the search
    query without reserializing the entire 'Atom' back to a 'B.ByteString'.
-}

{-# LANGUAGE OverloadedStrings #-}

module Atom
    ( -- * Types
      Prefix
    , Suffix
    , Atom(..)

      -- * Functions
    , atomToPage
    , atomToRecord
    , distSqA
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(rnf))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Format (fixed, format, left)
import Element (Element)
import Foreign.Safe (
    Storable(sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff),
    copyBytes,
    plusPtr )
import HSerialize (HSerialize(get, put), Store(Store, unStore))
import AtomName (AtomName)
import Point (Point(x, y, z), distSq)
import Page (Page, Size, pointToPage)

-- | Prefix of an ATOM record preceding the coordinates
type Prefix  = B.ByteString

-- | Suffix of an ATOM record following the coordinates
type Suffix  = B.ByteString

-- | Atom parsed from a PDB file
data Atom = Atom {
    name    :: {-# UNPACK #-} !AtomName,
    point   :: {-# UNPACK #-} !Point   ,
    element :: {-# UNPACK #-} !Element ,
    prefix  :: {-# UNPACK #-} !Prefix  ,
    suffix  :: {-# UNPACK #-} !Suffix  }
    deriving (Show)

instance HSerialize Atom where
    put = put . Store
    get = fmap unStore get

instance NFData Atom

-- | Convert an 'Atom' back to an ATOM record
atomToRecord :: Atom -> B.ByteString
atomToRecord a
  = let fmt dim = left 8 ' ' . fixed 3 . dim . point $ a
        coord
            = B.concat
            . BL.toChunks
            . encodeUtf8
            $ format "{}{}{}" (fmt x, fmt y, fmt z)
     in prefix a <> coord <> suffix a

-- | Compute the distance between two 'Atom's
distSqA :: Atom -> Atom -> Double
distSqA = distSq `on` point
{-# INLINE distSqA #-}

-- | Map an 'Atom' to a 'Page' using the global page 'Size'
atomToPage :: Size -> Atom -> Page
atomToPage s = pointToPage s . point

#{include <stdint.h>}
#{include "Point_hsc.h"}

#def typedef struct {
    uint64_t   name;
    point         r;
    uint8_t    elem;
    char prefix[30];
    char suffix[26];
} atom;

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Atom where
    sizeOf    _ = #{size      atom}
    alignment _ = #{alignment atom}
    peek p = Atom
     <$> #{peek atom, name} p
     <*> #{peek atom, r   } p
     <*> #{peek atom, elem} p
     <*> (B.packCStringLen (#{ptr atom, prefix} p, 30))
     <*> (B.packCStringLen (#{ptr atom, suffix} p, 26))
    poke p (Atom name r elem prefix suffix) = do
        #{poke atom, name} p name
        #{poke atom, r   } p r
        #{poke atom, elem} p elem
        B.useAsCStringLen prefix $ \(cstr, len) ->
            if (len == 30)
            then copyBytes (#{ptr atom, prefix} p) cstr 30
            else ioError $ userError "Atom: length prefix != 30"
        B.useAsCStringLen suffix $ \(cstr, len) ->
            if (len == 26)
            then copyBytes (#{ptr atom, suffix} p) cstr 26
            else ioError $ userError "Atom: length suffix != 26"
