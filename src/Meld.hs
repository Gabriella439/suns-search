{-| 'meld' is a small hack to work around the fact that users can build search
    queries from search results, which will give rise to overlapping atoms in
    the query which are supposed to be the same atom.

    For example, if a user searches for a carboxylic acid originating from an
    aspartate, there will be several valid linkers to choose from that connect
    to that carboxylic acid.  However, if the user selects an aspartic acid
    linker, the search query will have two Cγs: one from the linker and one from
    the original carboxylic acid.  If you tokenize the query, the tokenizer will
    match the linker twice: once for each Cγ.  'meld' filters out overlapping
    atoms which are probably duplicates (i.e. same residue, same name, and
    within 1.0 Å of each other.

    There is no need to remove atoms that have different residue or atom names
    since these will not generate duplicate matches for motifs.  In fact, you
    don't want to remove these because then their correspond motifs will no
    longer be matched.  For example, if you searched for a carboxylic acid from
    an aspartate, but then connected a linker from a glutamate, this would
    generate an overlapping Cδ from the glutamate and Cγ from the carboxylic
    acid, neither of which share the same residue or atom name.  You wouldn't
    want to remove either of these because then one of the two motifs would no
    longer be matched by the tokenizer.
-}

module Meld
    ( -- * Meld Atoms
      meld
    ) where

import Atom(Atom(name), distSqA)
import Data.List (tails)

{- I chose this cutoff assuming no hydrogen atoms.  The smallest non-hydrogen
   bonds are C-C bonds of 1.2 Angstroms, so I rounded down to 1.0 to be safe -}
cutoff = 1.0 -- Angstroms
cutoffSq = cutoff * cutoff

{-| Remove clashing atoms that share the same 'AtomName' and are within 1.0
    Å of each other.  'meld' arbitrarily keeps the second atom (in list order)
    every time it detects a clash.
-}
meld :: [Atom] -> [Atom]
meld query = map fst . filter snd $ zipWith
    (\a1 rest ->
        (a1, all (\a2 -> distSqA a1 a2 > cutoffSq || name a1 /= name a2) rest))
    query
    (tail $ tails query)
