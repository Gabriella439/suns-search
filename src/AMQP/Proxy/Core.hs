-- | Low-level @pipes@ wrapper around an AMQP stream

module AMQP.Proxy.Core 
    ( -- * Low-level AMQP wrapper
      listen
    ) where

import AMQP.Types (QueueName)
import Control.Concurrent.STM (atomically)
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import qualified Network.AMQP as A
import Pipes (Producer, yield, for)
import Pipes.Concurrent (spawn, Buffer(Single), send, recv, forkIO, fromOutput)

{-| Listen to all messages from the given queue

    If 'A.Ack' is set, then 'listen' will auto-acknowledge all messages after
    each 'yield'.
-}
listen
    :: A.Connection
    -> A.Channel
    -> QueueName
    -> A.Ack
    -> IO (Producer (A.Message, A.Envelope) IO ())
listen connection channel queueName ack = do
    (input , output ) <- spawn Single
    (inKill, outKill) <- spawn Single
    consumerTag <- A.consumeMsgs channel queueName ack $ \payload -> do
        alive <- atomically $ send input payload
        when (not alive) $ void $ atomically $ send inKill ()
    forkIO $ do
        atomically $ recv outKill
        A.cancelConsumer channel consumerTag
    return $ for (fromOutput output) $ \payload@(_, envelope) -> do
        yield payload
        lift $ case ack of
            A.Ack   -> A.ackEnv envelope
            A.NoAck -> return ()
