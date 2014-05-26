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

-- | Shared AMQP initialization routines that do some additional logging

module AMQP.Error
    ( -- * Initialization Routines
      bindQueue
    , declareExchange
    , declareQueue
    ) where

import AMQP.Types (QueueName, ExchangeName, RoutingKey)
import Control.Monad (when)
import Data.Text (unpack)
import Log (debug, warn)
import qualified Network.AMQP as A

debugDeclareExchange :: ExchangeName -> String
debugDeclareExchange xName =
 "Declared exchange: \"" ++ unpack xName ++ "\""

{-| Wrapper around 'A.declareExchange' that outputs additional debug information
-}
declareExchange :: A.Channel -> A.ExchangeOpts -> IO ()
declareExchange channel exchangeOpts = do
    A.declareExchange channel exchangeOpts
    debug $ debugDeclareExchange (A.exchangeName exchangeOpts)

warnDeclareQueue :: QueueName -> QueueName -> String
warnDeclareQueue name name' =
 "'declareQueue' returned a new queue name\n\
 \    Requested queue name: '" ++ unpack name  ++ "'\n\
 \    Returned  queue name: '" ++ unpack name' ++ "'"

debugDeclareQueue :: QueueName -> Int -> Int -> String
debugDeclareQueue queueName numMsg numCons =
 "Declared queue: \"" ++ unpack queueName ++ "\"\n\
 \    " ++ show numMsg  ++ " messages in the queue\n\
 \    " ++ show numCons ++ " active consumers for the queue"

{-| Wrapper around 'A.declareQueue' that outputs additional debug or warning
    information
-}
declareQueue :: A.Channel -> A.QueueOpts -> IO ()
declareQueue channel queueOpts = do
    (qName', numMsg, numCons) <- A.declareQueue channel queueOpts
    let qName = A.queueName queueOpts
    when (qName' /= qName) $ warn $ warnDeclareQueue qName qName'
    debug $ debugDeclareQueue qName numMsg numCons

debugBindQueue :: QueueName -> ExchangeName -> RoutingKey -> String
debugBindQueue qName xName routingKey =
 "Bound queue: \"" ++ unpack qName ++ "\"\n\
 \    to exchange: \"" ++ unpack xName      ++ "\"\n\
 \    with key   : \"" ++ unpack routingKey ++ "\""

-- | Wrapper around 'A.bindQueue' that outputs additional debug information
bindQueue :: A.Channel -> QueueName -> ExchangeName -> RoutingKey -> IO ()
bindQueue channel qName xName routingKey = do
    A.bindQueue channel qName xName routingKey
    debug $ debugBindQueue qName xName routingKey
