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

{-| High-level Suns-specific AMQP routines designed to interface with the types
    the search engine requires.
-}

{-# LANGUAGE OverloadedStrings #-}

module AMQP.Proxy
    ( -- * AMQP Interaction
      Version
    , AMQPHandle(..)
    , withAMQP
    ) where

import qualified AMQP.Error as AE
import AMQP.Proxy.Core (listen)
import AMQP.Types (CorrelationID, ExchangeName, HostName, RoutingKey)
import Atom (Atom, atomToRecord)
import Control.Applicative ((<$>), (<*>))
import Control.Error (note)
import Control.Exception (bracket)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Aeson (decode')
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid ((<>))
import Data.Text (Text)
import Log (debug, warn)
import qualified Network.AMQP as A
import qualified Network.AMQP.Types as A
import Password (Password, getPassword)
import PDB (PDBID)
import Pipes (Producer, yield, for, every)
import Request (Request)
import Search (Response(..))

{-| A 'Text' string identifying the protocol version, used to connect clients
    with the appropriate search engine when necessary to support old protocols.
-}
type Version = Text

validMessage :: A.Message -> MaybeT IO ((RoutingKey, CorrelationID), Request)
validMessage message = MaybeT $ do
    let correlationID = A.msgCorrelationID message
        e = (,)
         <$> (note "Message missing \"replyTo\" field" $ A.msgReplyTo message)
         <*> (note "Message parse failed" $ decode' $ A.msgBody $ message)
    case e of
        Left   str           -> do
            warn str
            return Nothing
        Right (replyTo, req) -> return $ Just ((replyTo, correlationID), req)

publish
    :: A.Channel
    -> ExchangeName
    -> ((RoutingKey, CorrelationID), Response (PDBID, [Atom]))
    -> IO ()
publish channel exchangeName ((routingKey, correlationID), mAtoms) = do
    let body = case mAtoms of
            Done                  -> BL.singleton '0'
            Result (pdbID, atoms) -> BL.concat
                [ BL.singleton '1'
                , BL.pack pdbID
                , BL.fromChunks [BS.unlines $ map atomToRecord atoms]
                ]
            Timeout               -> BL.singleton '2'
            Error str             -> BL.cons '3' (BL.pack str)
        message = A.Message
            body          -- msgBody
            Nothing       -- msgDeliveryMode
            Nothing       -- msgTimeStamp
            Nothing       -- msgID
            Nothing       -- msgContentType
            Nothing       -- msgReplyTo
            correlationID -- msgCorrelationID
            Nothing       -- msgHeaders
    A.publishMsg channel exchangeName routingKey message

{-| A high-level wrapper around an AMQP connection providing a way to read
    client requests and respond with search results
-}
data AMQPHandle = AMQPHandle
    { requests
        :: Producer ((RoutingKey, CorrelationID), Request) IO ()
      -- ^ A 'Producer' that outputs client requests from the message queue,
      --   terminating if the connection is lost.
    , respond
        :: ((RoutingKey, CorrelationID), Response (PDBID, [Atom])) -> IO ()
      -- ^ Command to publish a search result to the message queue
    }

{-| Safely acquire an 'AMQPHandle'

    Requires the server address, message queue password for @suns-server@ and
    the protocol version
-}
withAMQP :: HostName -> Password -> Version -> (AMQPHandle -> IO r) -> IO r
withAMQP hostName password version k = bracket
    (do connection <- A.openConnection
            hostName
            "suns-vhost"
            "suns-server"
            (getPassword password)
        channel <- A.openChannel connection
    
        let xReqs  = "suns-exchange-requests"
        AE.declareExchange channel $ A.newExchange
            { A.exchangeName       = xReqs
            , A.exchangeType       = "direct"
            , A.exchangePassive    = True
            , A.exchangeDurable    = True
            , A.exchangeAutoDelete = False
            , A.exchangeInternal   = False
            }

        let xResps = "suns-exchange-responses"
        AE.declareExchange channel $ A.newExchange
            { A.exchangeName       = xResps
            , A.exchangeType       = "direct"
            , A.exchangePassive    = True
            , A.exchangeDurable    = True
            , A.exchangeAutoDelete = False
            , A.exchangeInternal   = False
            }
    
        let qName = "suns-queue-" <> version
        AE.declareQueue channel $ A.QueueOpts
            qName                   -- queueName
            True                    -- queuePassive
            True                    -- queueDurable
            False                   -- queueExclusive
            False                   -- queueAutoDelete
            (A.FieldTable M.empty)  -- queueHeaders
    
        listener <- listen connection channel qName A.Ack
        let close = A.closeConnection connection
        return
            ( AMQPHandle
                { requests = for listener $ \(msg, _) -> do
--                   lift $ debug $ (++ "\n") $ BL.unpack $ A.msgBody msg
--                   lift $ debug "Waiting for a request"
                     every (validMessage msg)
                , respond = publish channel xResps
                }
            , close
            ) )
    (\(_     , close) -> close   )
    (\(handle, _    ) -> k handle)
