-- | Shared logging routines

module Log
    ( -- * Initialization
      initLog

      -- * Logging
    , debug
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
    h <- streamHandler stderr L.DEBUG
    let h' = setFormatter h (simpleLogFormatter "[$time] $msg") 
    L.updateGlobalLogger L.rootLoggerName $
        L.setLevel L.DEBUG . L.addHandler h' . L.setHandlers [s]

log :: L.Priority -> String -> IO ()
log = L.logM L.rootLoggerName

-- | Log at 'L.DEBUG' level
debug :: String -> IO ()
debug = log L.DEBUG

-- | Log at 'L.WARN' level
warn :: String -> IO ()
warn = log L.WARNING

-- | Log at 'L.EMERGENCY' level
emergency :: String -> IO ()
emergency = log L.EMERGENCY
