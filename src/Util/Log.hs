-- | A simplified interface to the hslogger module.
module Util.Log (
    initLogger,
    debugM,
    infoM,
    noticeM ,
    warningM,
    errorM,
    criticalM,
    alertM,
    emergencyM,
    Priority(..)
) where

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import Data.Time (getZonedTime)
import Data.Time.Format (FormatTime, formatTime)
import System.Locale (defaultTimeLocale)

-- | Creates the logfile @.\/Logs\/MyS-YYYY-MM-DD-HH:mm:ss.log@
--
--   Logging level set to DEBUG.
initLogger :: Priority -> IO ()
initLogger level = do
    -- Time
    time <- getZonedTime

    -- Growl logger
    file  <- openLog ("./Logs/MyS-" ++ isoDatetime time ++ ".log")
    growl <- verboseStreamHandler file DEBUG
    
    -- Set global logger
    updateGlobalLogger rootLoggerName (setHandlers [growl])
    
    -- Set global loglevel
    updateGlobalLogger rootLoggerName (setLevel level)
  where
    openLog path = openFile path AppendMode
    isoDatetime = formatTime defaultTimeLocale "%F-%X"