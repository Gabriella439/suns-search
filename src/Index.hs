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

import Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as Ex
import Data.Monoid (mconcat)
import HSerialize (encodeFile)
import Log (initLog, emergency)
import Motif (numMotifs, motifsFromDir)
import qualified Options.Applicative as O
import Indices (primaryIndex, secondaryIndex)

options :: O.Parser (String, String, String)
options = (,,)
 <$> (O.strOption $ mconcat
    [ O.short 'm'
    , O.long "motifs"
    , O.metavar "MOTIFDIR"
    , O.value "motif/"
    , O.showDefaultWith id
    , O.completer (O.bashCompleter "directory")
    , O.help "Input motif directory"
    ] )
 <*> (O.strOption $ mconcat
    [ O.short 'p'
    , O.long "pdbs"
    , O.metavar "PDBDIR"
    , O.value "pdb/"
    , O.showDefaultWith id
    , O.help "Input PDB directory"
    ] )
 <*> (O.strOption $ mconcat
    [ O.short 'i'
    , O.long "index"
    , O.metavar "INDEXDIR"
    , O.value "index/"
    , O.showDefaultWith id
    , O.completer (O.bashCompleter "directory")
    , O.help "Output index directory"
    ] )

main = (do
    (motifDir, pdbDir, indexDir) <-
        O.execParser $ O.info (O.helper <*> options) $ mconcat
            [ O.fullDesc
            , O.header "The Suns structural search engine"
            , O.progDesc "Indexes PDB structures using the provided motifs"
            , O.footer "Report bugs to Gabriel439@gmail.com"
            ]
    initLog
    motifs <- motifsFromDir motifDir
    i2 <- secondaryIndex 0 pdbDir motifs
    let i1 = primaryIndex (numMotifs motifs) i2
    encodeFile (indexDir ++ "/index_primary.dat"  ) i1
    encodeFile (indexDir ++ "/index_secondary.dat") i2
    encodeFile (indexDir ++ "/motifs.dat"         ) motifs )
  `Ex.catch` (\e -> do
    emergency $ show (e :: Ex.IOException)
    Ex.throwIO e )
