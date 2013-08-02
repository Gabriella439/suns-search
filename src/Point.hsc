-- | Utilities for 3-dimensional points

module Point
    ( -- * Type
      Point(..)

      -- * Functions
    , distSq
    , listToPoint
    , pointToList
    ) where

import Control.Applicative ((<$>), (<*>))
import Foreign.Safe (
    Storable(sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff) )

-- | 3-dimensional point
data Point = Point {
    x :: {-# UNPACK #-} !Double,
    y :: {-# UNPACK #-} !Double,
    z :: {-# UNPACK #-} !Double}
    deriving (Show)

-- | Compute the distance between two 'Point's
distSq :: Point -> Point -> Double
distSq (Point x1 y1 z1) (Point x2 y2 z2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
     in dx * dx + dy * dy + dz * dz

-- | Convert a 'Point' to a list of three coordinates
pointToList :: Point -> [Double]
pointToList (Point x y z) = [x, y, z]

{-| Convert a list of three coordinates to a 'Point' or return 'Nothing' if the
    list does not have exactly three values
-}
listToPoint :: [Double] -> Maybe Point
listToPoint cs = case cs of
    [x, y, z] -> Just $ Point x y z
    _         -> Nothing

#def typedef struct {
    double x;
    double y;
    double z;
} point;

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Point where
    sizeOf    _ = #{size      point}
    alignment _ = #{alignment point}
    peek p = Point
     <$> #{peek point, x} p
     <*> #{peek point, y} p
     <*> #{peek point, z} p
    poke p (Point x y z) = do
        #{poke point, x} p x
        #{poke point, y} p y
        #{poke point, z} p z
