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

-- | Shared logging routines

module Log
    ( -- * Initialization
      initLog

      -- * Logging
    , debug
    , info
    , warn
    , emergency
    ) where

import qualified System.Log.Logger as L
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Syslog (openlog, Option(PID), Facility(DAEMON))
import System.Log.Handler.Simple (streamHandler)
import System.IO (stderr)
import Prelude hiding (log)

{-| Initialize the logger, setting it up to log to @syslog@ and @stderr@

    Must be called before other routines in this module
-}
initLog :: IO ()
initLog = do
    s <- openlog "suns-1.0.0" [PID] DAEMON L.WARNING
    h <- streamHandler stderr L.INFO
    let h' = setFormatter h (simpleLogFormatter "[$time] $msg") 
    L.updateGlobalLogger L.rootLoggerName $
        L.setLevel L.INFO . L.addHandler h' . L.setHandlers [s]

log :: L.Priority -> String -> IO ()
log = L.logM L.rootLoggerName

-- | Log at 'L.DEBUG' level
debug :: String -> IO ()
debug = log L.DEBUG

-- | Log at 'L.INFO' level
info :: String -> IO ()
info = log L.INFO

-- | Log at 'L.WARN' level
warn :: String -> IO ()
warn = log L.WARNING

-- | Log at 'L.EMERGENCY' level
emergency :: String -> IO ()
emergency = log L.EMERGENCY
