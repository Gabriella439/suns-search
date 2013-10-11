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

-- | Top-level module for the server

module Main
    ( -- * Main
      main
    ) where

import AMQP.Proxy (withAMQP, AMQPHandle(AMQPHandle))
import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (deepseq)
import qualified Control.Exception as Ex
import Data.Monoid (mconcat)
import qualified Data.Text as T
import HSerialize (decodeFile)
import Log (initLog, emergency)
import qualified Options.Applicative as O
import Password (password)
import Pipes
import Search (search)
import Structure () -- NFData Instance

options :: O.Parser (String, String, Maybe Integer, T.Text)
options = (,,,)
 <$> (O.strOption $ mconcat
    [ O.short 'n'
    , O.long "node"
    , O.metavar "NODE"
    , O.value "127.0.0.1"
    , O.showDefaultWith id
    , O.completer (O.bashCompleter "hostname")
    , O.help "AMQP node to connect to"
    ] )
 <*> (O.strOption $ mconcat
    [ O.short 'i'
    , O.long "index"
    , O.metavar "DIR"
    , O.value "index/"
    , O.showDefaultWith id
    , O.completer (O.bashCompleter "directory")
    , O.help "Index directory"
    ] )
 <*> (O.nullOption $ mconcat
    [ O.reader (fmap Just . O.auto)
    , O.short 't'
    , O.long "timeout"
    , O.metavar "MILLISECONDS"
    , O.value Nothing
    , O.showDefaultWith (\_ -> "No timeout")
    , O.help "Time limit for requests"
    ] )
 <*> (fmap T.pack $ O.strOption $ mconcat
    [ O.short 'q'
    , O.long "queue"
    , O.metavar "QUEUE"
    , O.value "1.0.0"
    , O.showDefaultWith id
    , O.help "Queue to serve"
    ] )

{-| Runs a long-lived search engine that connects to a message queue to receive
    search requests and reply with search responses
-}
main = (do
    (hostName, indexDir, timeout, version) <-
        O.execParser $ O.info (O.helper <*> options) $ mconcat
            [ O.fullDesc
            , O.header "The Suns structural search engine"
            , O.progDesc
                "Connects to a message queue and handles search requests"
            , O.footer "Report bugs to Gabriel439@gmail.com"
            ]

    initLog

    pwd <- password

    motifs <- decodeFile (indexDir ++ "/motifs.dat"         )
    i1     <- decodeFile (indexDir ++ "/index_primary.dat"  )
    i2     <- decodeFile (indexDir ++ "/index_secondary.dat")
    motifs `deepseq` i1 `deepseq` i2 `deepseq` return ()

    withAMQP hostName pwd version $ \(AMQPHandle requests respond) ->
        runEffect $ for requests (search motifs i1 i2 timeout ~> lift . respond)
    emergency "Server connection lost" )
  `Ex.catch` (\e -> do
    emergency $ show (e :: Ex.IOException)
    )
