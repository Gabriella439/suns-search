-- | Shared AMQP initialization routines that do some additional logging

module AMQP.Error
    ( -- * Initialization Routines
      bindQueue
    , declareExchange
    , declareQueue
    ) where

import AMQP.Types (QueueName, ExchangeName, RoutingKey)
import Control.Monad (when)
import qualified Data.Text as T
import Log (debug, warn)
import qualified Network.AMQP as A

debugDeclareExchange :: ExchangeName -> String
debugDeclareExchange xName =
 "Declared exchange: \"" ++ T.unpack xName ++ "\""

{-| Wrapper around 'A.declareExchange' that outputs additional debug information
-}
declareExchange :: A.Channel -> A.ExchangeOpts -> IO ()
declareExchange channel exchangeOpts = do
    A.declareExchange channel exchangeOpts
    debug $ debugDeclareExchange (A.exchangeName exchangeOpts)

warnDeclareQueue :: QueueName -> QueueName -> String
warnDeclareQueue name name' =
 "'declareQueue' returned a new queue name\n\
 \    Requested queue name: '" ++ T.unpack name  ++ "'\n\
 \    Returned  queue name: '" ++ T.unpack name' ++ "'"

debugDeclareQueue :: QueueName -> Int -> Int -> String
debugDeclareQueue queueName numMsg numCons =
 "Declared queue: \"" ++ T.unpack queueName ++ "\"\n\
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
 "Bound queue: \"" ++ T.unpack qName ++ "\"\n\
 \    to exchange: \"" ++ T.unpack xName      ++ "\"\n\
 \    with key   : \"" ++ T.unpack routingKey ++ "\""

-- | Wrapper around 'A.bindQueue' that outputs additional debug information
bindQueue :: A.Channel -> QueueName -> ExchangeName -> RoutingKey -> IO ()
bindQueue channel qName xName routingKey = do
    A.bindQueue channel qName xName routingKey
    debug $ debugBindQueue qName xName routingKey
