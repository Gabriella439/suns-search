-- | Shared AMQP types

module AMQP.Types
    ( -- * AMQP Types
      CorrelationID
    , ExchangeName
    , HostName
    , QueueName
    , RoutingKey
    ) where

import Data.Text (Text)

-- | Correlation ID, used to match responses to requests
type CorrelationID = Maybe Text

-- | AMQP exchange name
type ExchangeName  = Text

-- | AMQP server address
type HostName      = String

-- | Routing key, used to specify the destination of published messages
type RoutingKey    = Text

-- | AMQP message queue
type QueueName     = Text
