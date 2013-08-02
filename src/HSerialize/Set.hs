-- | 'HSerialize' instance for 'Set'

module HSerialize.Set () where

import qualified Data.Set as S
import HSerialize.Core (HSerialize (put, get))

instance (HSerialize a) => HSerialize (S.Set a) where
    put = put . S.toAscList
    get = fmap S.fromDistinctAscList get
