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
