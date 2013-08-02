{-| The primary index for the search engine, responsible for locating all pages
    that match the search query purely on word count
-}

module Primary
    ( -- * Type
      PrimaryIndex

      -- * Indexing
    , primaryIndex

      -- * Querying
    , queryPrimary
    ) where

import Control.Arrow (second)
import Control.Error (maximumDef)
import Control.DeepSeq (NFData)
import qualified Data.HashMap.Strict as H
import Data.Monoid (Monoid(mempty))
import qualified Data.Set as S
import qualified Data.Vector as V
import HSerialize (HSerialize(get, put))

newtype PrimaryIndex = PrimaryIndex
    { unPrimaryIndex :: V.Vector (V.Vector (S.Set Int)) }

instance NFData PrimaryIndex

instance HSerialize PrimaryIndex where
    get = fmap PrimaryIndex get
    put = put . unPrimaryIndex

{-| Given a 'V.Vector' of tokenized structures, return the primary index.  The
    'Int' parameter is the number of motifs, which I pass as a programming
    convenience to avoid having to extract this number from the first entry.
-}
primaryIndex
 :: Int
 -> V.Vector (pdbID, (atoms, V.Vector (V.Vector match)))
 -> PrimaryIndex
primaryIndex numMotif
  = PrimaryIndex
  . V.map (V.drop 1 . countVector)
  . V.foldr (V.zipWith (++)) (V.replicate numMotif [])
  . V.imap (\i (_, (_, v)) -> V.map (\matches -> [(V.length matches, i)]) v)

countVector :: (Ord a) => [(Int, a)] -> V.Vector (S.Set a)
countVector
  = hashmapToVector
  . H.map S.fromList
  . H.fromListWith (++)
  . map (second return)

hashmapToVector :: (Monoid a) => H.HashMap Int a -> V.Vector a
hashmapToVector h =
    let nMax = maximumDef 0 $ H.keys h
        v = V.replicate (nMax + 1) mempty
     in v V.// H.toList h

{-| Query the 'PrimaryIndex' using a tokenized structure, returning a 'S.Set'
    of all pages that matched.

    Returns 'Nothing' if the tokenized structure was empty and therefore matched
    every page trivially.
-}
queryPrimary
 :: PrimaryIndex
 -> V.Vector (V.Vector match)
 -> Maybe (S.Set Int)
queryPrimary (PrimaryIndex i1) iq
  = V.foldl'
        (\acc elem -> case acc of
            Nothing -> elem
            Just s1 -> case elem of
                Nothing -> acc
                Just s2 -> Just $ S.intersection s1 s2 )
        Nothing
  $ V.zipWith
        (\incidences count ->
            if (count < 1)
            then Nothing
            else Just
               . V.foldl' S.union S.empty
               . V.drop (count - 1)
               $ incidences)
        i1
        (V.map V.length iq)
