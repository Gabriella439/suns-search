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
import Pipes.Concurrent (spawn, Buffer(Single), send, recv, forkIO, fromInput)

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
    (output , input ) <- spawn Single
    (outKill, inKill) <- spawn Single
    consumerTag <- A.consumeMsgs channel queueName ack $ \payload -> do
        alive <- atomically $ send output payload
        when (not alive) $ void $ atomically $ send outKill ()
    forkIO $ do
        atomically $ recv inKill
        A.cancelConsumer channel consumerTag
    return $ for (fromInput input) $ \payload@(_, envelope) -> do
        yield payload
        lift $ case ack of
            A.Ack   -> A.ackEnv envelope
            A.NoAck -> return ()
