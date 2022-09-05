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

-- | Interconversion routines between 'Element's and 'B.ByteString's

module Element
    ( -- * Type
      Element

      -- * Conversion routines
    , bsToElem
    , elemToBs
    ) where

import qualified Data.ByteString as B
import Data.Word (Word8)

-- | 256 elements ought to be enough for anybody
type Element = Word8

-- I auto-generated the following case statements using a table of elements

-- | Convert an two-character ASCII 'B.ByteString' to an 'Element'
bsToElem :: B.ByteString -> Maybe Element
bsToElem bs = case B.unpack bs of
    [32,72] -> Just 0
    [72,69] -> Just 1
    [76,73] -> Just 2
    [66,69] -> Just 3
    [32,66] -> Just 4
    [32,67] -> Just 5
    [32,78] -> Just 6
    [32,79] -> Just 7
    [32,70] -> Just 8
    [78,69] -> Just 9
    [78,65] -> Just 10
    [77,71] -> Just 11
    [65,76] -> Just 12
    [83,73] -> Just 13
    [32,80] -> Just 14
    [32,83] -> Just 15
    [67,76] -> Just 16
    [65,82] -> Just 17
    [32,75] -> Just 18
    [67,65] -> Just 19
    [83,67] -> Just 20
    [84,73] -> Just 21
    [32,86] -> Just 22
    [67,82] -> Just 23
    [77,78] -> Just 24
    [70,69] -> Just 25
    [67,79] -> Just 26
    [78,73] -> Just 27
    [67,85] -> Just 28
    [90,78] -> Just 29
    [71,65] -> Just 30
    [71,69] -> Just 31
    [65,83] -> Just 32
    [83,69] -> Just 33
    [66,82] -> Just 34
    [75,82] -> Just 35
    [82,66] -> Just 36
    [83,82] -> Just 37
    [32,89] -> Just 38
    [90,82] -> Just 39
    [78,66] -> Just 40
    [77,79] -> Just 41
    [84,67] -> Just 42
    [82,85] -> Just 43
    [82,72] -> Just 44
    [80,68] -> Just 45
    [65,71] -> Just 46
    [67,68] -> Just 47
    [73,78] -> Just 48
    [83,78] -> Just 49
    [83,66] -> Just 50
    [84,69] -> Just 51
    [32,73] -> Just 52
    [88,69] -> Just 53
    [67,83] -> Just 54
    [66,65] -> Just 55
    [76,65] -> Just 56
    [67,69] -> Just 57
    [80,82] -> Just 58
    [78,68] -> Just 59
    [80,77] -> Just 60
    [83,77] -> Just 61
    [69,85] -> Just 62
    [71,68] -> Just 63
    [84,66] -> Just 64
    [68,89] -> Just 65
    [72,79] -> Just 66
    [69,82] -> Just 67
    [84,77] -> Just 68
    [89,66] -> Just 69
    [76,85] -> Just 70
    [72,70] -> Just 71
    [84,65] -> Just 72
    [32,87] -> Just 73
    [82,69] -> Just 74
    [79,83] -> Just 75
    [73,82] -> Just 76
    [80,84] -> Just 77
    [65,85] -> Just 78
    [72,71] -> Just 79
    [84,76] -> Just 80
    [80,66] -> Just 81
    [66,73] -> Just 82
    [80,79] -> Just 83
    [65,84] -> Just 84
    [82,78] -> Just 85
    [70,82] -> Just 86
    [82,65] -> Just 87
    [65,67] -> Just 88
    [84,72] -> Just 89
    [80,65] -> Just 90
    [32,85] -> Just 91
    [78,80] -> Just 92
    [80,85] -> Just 93
    [65,77] -> Just 94
    [67,77] -> Just 95
    [66,75] -> Just 96
    [67,70] -> Just 97
    [69,83] -> Just 98
    [70,77] -> Just 99
    [77,68] -> Just 100
    [78,79] -> Just 101
    [76,82] -> Just 102
    [82,70] -> Just 103
    [68,66] -> Just 104
    [83,71] -> Just 105
    [66,72] -> Just 106
    [72,83] -> Just 107
    [77,84] -> Just 108
    [68,83] -> Just 109
    [82,71] -> Just 110
    [67,78] -> Just 111
    _       -> Nothing

-- | Convert an 'Element' to a two-character ASCII 'B.ByteSTring'
elemToBs :: Element -> Maybe B.ByteString
elemToBs e = case e of
    0   -> Just $ B.pack [32,72]
    1   -> Just $ B.pack [72,69]
    2   -> Just $ B.pack [76,73]
    3   -> Just $ B.pack [66,69]
    4   -> Just $ B.pack [32,66]
    5   -> Just $ B.pack [32,67]
    6   -> Just $ B.pack [32,78]
    7   -> Just $ B.pack [32,79]
    8   -> Just $ B.pack [32,70]
    9   -> Just $ B.pack [78,69]
    10  -> Just $ B.pack [78,65]
    11  -> Just $ B.pack [77,71]
    12  -> Just $ B.pack [65,76]
    13  -> Just $ B.pack [83,73]
    14  -> Just $ B.pack [32,80]
    15  -> Just $ B.pack [32,83]
    16  -> Just $ B.pack [67,76]
    17  -> Just $ B.pack [65,82]
    18  -> Just $ B.pack [32,75]
    19  -> Just $ B.pack [67,65]
    20  -> Just $ B.pack [83,67]
    21  -> Just $ B.pack [84,73]
    22  -> Just $ B.pack [32,86]
    23  -> Just $ B.pack [67,82]
    24  -> Just $ B.pack [77,78]
    25  -> Just $ B.pack [70,69]
    26  -> Just $ B.pack [67,79]
    27  -> Just $ B.pack [78,73]
    28  -> Just $ B.pack [67,85]
    29  -> Just $ B.pack [90,78]
    30  -> Just $ B.pack [71,65]
    31  -> Just $ B.pack [71,69]
    32  -> Just $ B.pack [65,83]
    33  -> Just $ B.pack [83,69]
    34  -> Just $ B.pack [66,82]
    35  -> Just $ B.pack [75,82]
    36  -> Just $ B.pack [82,66]
    37  -> Just $ B.pack [83,82]
    38  -> Just $ B.pack [32,89]
    39  -> Just $ B.pack [90,82]
    40  -> Just $ B.pack [78,66]
    41  -> Just $ B.pack [77,79]
    42  -> Just $ B.pack [84,67]
    43  -> Just $ B.pack [82,85]
    44  -> Just $ B.pack [82,72]
    45  -> Just $ B.pack [80,68]
    46  -> Just $ B.pack [65,71]
    47  -> Just $ B.pack [67,68]
    48  -> Just $ B.pack [73,78]
    49  -> Just $ B.pack [83,78]
    50  -> Just $ B.pack [83,66]
    51  -> Just $ B.pack [84,69]
    52  -> Just $ B.pack [32,73]
    53  -> Just $ B.pack [88,69]
    54  -> Just $ B.pack [67,83]
    55  -> Just $ B.pack [66,65]
    56  -> Just $ B.pack [76,65]
    57  -> Just $ B.pack [67,69]
    58  -> Just $ B.pack [80,82]
    59  -> Just $ B.pack [78,68]
    60  -> Just $ B.pack [80,77]
    61  -> Just $ B.pack [83,77]
    62  -> Just $ B.pack [69,85]
    63  -> Just $ B.pack [71,68]
    64  -> Just $ B.pack [84,66]
    65  -> Just $ B.pack [68,89]
    66  -> Just $ B.pack [72,79]
    67  -> Just $ B.pack [69,82]
    68  -> Just $ B.pack [84,77]
    69  -> Just $ B.pack [89,66]
    70  -> Just $ B.pack [76,85]
    71  -> Just $ B.pack [72,70]
    72  -> Just $ B.pack [84,65]
    73  -> Just $ B.pack [32,87]
    74  -> Just $ B.pack [82,69]
    75  -> Just $ B.pack [79,83]
    76  -> Just $ B.pack [73,82]
    77  -> Just $ B.pack [80,84]
    78  -> Just $ B.pack [65,85]
    79  -> Just $ B.pack [72,71]
    80  -> Just $ B.pack [84,76]
    81  -> Just $ B.pack [80,66]
    82  -> Just $ B.pack [66,73]
    83  -> Just $ B.pack [80,79]
    84  -> Just $ B.pack [65,84]
    85  -> Just $ B.pack [82,78]
    86  -> Just $ B.pack [70,82]
    87  -> Just $ B.pack [82,65]
    88  -> Just $ B.pack [65,67]
    89  -> Just $ B.pack [84,72]
    90  -> Just $ B.pack [80,65]
    91  -> Just $ B.pack [32,85]
    92  -> Just $ B.pack [78,80]
    93  -> Just $ B.pack [80,85]
    94  -> Just $ B.pack [65,77]
    95  -> Just $ B.pack [67,77]
    96  -> Just $ B.pack [66,75]
    97  -> Just $ B.pack [67,70]
    98  -> Just $ B.pack [69,83]
    99  -> Just $ B.pack [70,77]
    100 -> Just $ B.pack [77,68]
    101 -> Just $ B.pack [78,79]
    102 -> Just $ B.pack [76,82]
    103 -> Just $ B.pack [82,70]
    104 -> Just $ B.pack [68,66]
    105 -> Just $ B.pack [83,71]
    106 -> Just $ B.pack [66,72]
    107 -> Just $ B.pack [72,83]
    108 -> Just $ B.pack [77,84]
    109 -> Just $ B.pack [68,83]
    110 -> Just $ B.pack [82,71]
    111 -> Just $ B.pack [67,78]
    _   -> Nothing
