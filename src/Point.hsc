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
pointToList (Point x_ y_ z_) = [x_, y_, z_]

{-| Convert a list of three coordinates to a 'Point' or return 'Nothing' if the
    list does not have exactly three values
-}
listToPoint :: [Double] -> Maybe Point
listToPoint cs = case cs of
    [x_, y_, z_] -> Just $ Point x_ y_ z_
    _            -> Nothing

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
    poke p (Point x_ y_ z_) = do
        #{poke point, x} p x_
        #{poke point, y} p y_
        #{poke point, z} p z_
