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

-- | Top-level module for the @suns-admin@ program

{-# LANGUAGE OverloadedStrings #-}

module Main (
    -- * Main
    main  
    ) where

import AMQP.Types (HostName, QueueName)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Data.Monoid (mconcat, (<>))
import Data.Text (pack)
import qualified Network.AMQP as A
import qualified Options.Applicative as O

import qualified AMQP.Error as AE
import Log (initLog)
import Password (getPassword, password)

options :: O.Parser (HostName, QueueName)
options = (,)
    <$> (O.strOption $ mconcat [
        O.short 'n',
        O.long "node",
        O.metavar "NODE",
        O.value "127.0.0.1",
        O.showDefaultWith id,
        O.completer (O.bashCompleter "hostname"),
        O.help "AMQP node to connect to" ] )
    <*> (fmap pack $ O.strOption $ mconcat [
        O.short 'q',
        O.long "queue",
        O.metavar "QUEUE",
        O.value "1.0.0",
        O.showDefaultWith id,
        O.help "Queue to initialize" ] )

parserInfo :: O.ParserInfo (HostName, QueueName)
parserInfo = O.info (O.helper <*> options) $ mconcat
    [ O.fullDesc
    , O.header "The Suns setup utility"
    , O.progDesc "Sets up an AMQP node"
    , O.footer "Report bugs to Gabriella439@gmail.com"
    ]

{-| Sets up the RabbitMQ server with the appropriate virtual host, exchanges,
    and queues
-}
main :: IO ()
main = do
    (hostName, queue) <- O.execParser parserInfo

    initLog

    pwd <- password

    bracket
        (A.openConnection hostName "suns-vhost" "suns-admin" (getPassword pwd))
        A.closeConnection
        $ \connection -> do
            channel <- A.openChannel connection
        
            let xName1 = "suns-exchange-requests"
            AE.declareExchange channel $ A.newExchange
                { A.exchangeName       = xName1
                , A.exchangeType       = "direct"
                , A.exchangePassive    = False
                , A.exchangeDurable    = True
                , A.exchangeAutoDelete = False
                , A.exchangeInternal   = False
                }

            let xName2 = "suns-exchange-responses"
            AE.declareExchange channel $ A.newExchange
                { A.exchangeName       = xName2
                , A.exchangeType       = "direct"
                , A.exchangePassive    = False
                , A.exchangeDurable    = True
                , A.exchangeAutoDelete = False
                , A.exchangeInternal   = False
                }
        
            let qName = "suns-queue-" <> queue
            AE.declareQueue channel $ A.newQueue
                { A.queueName       = qName
                , A.queuePassive    = False
                , A.queueDurable    = True
                , A.queueExclusive  = False
                , A.queueAutoDelete = False
                }
        
            AE.bindQueue channel qName xName1 queue
