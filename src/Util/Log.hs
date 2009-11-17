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

-- File handling
import System.IO

-- Logging
import System.Log.Logger
import System.Log.Handler.Simple

-- Log file naming
import Data.Time (getZonedTime)
import Data.Time.Format (FormatTime, formatTime)
import System.Locale (defaultTimeLocale)

{- | A simplified interface to the hslogger module. -}

-- | Creates the logfile: ./Logs/My-YYYY-MM-DD-HH:mm:ss.log
--   Logging level set to DEBUG.
initLogger :: Priority -> IO ()
initLogger level = do
    -- Time
    time <- getZonedTime

    -- Growl logger
    file  <- openLog ("./Logs/My-" ++ isoDatetime time ++ ".log")
    growl <- verboseStreamHandler file DEBUG
    
    -- Set global logger
    updateGlobalLogger rootLoggerName (setHandlers [growl])
    
    -- Set global loglevel
    updateGlobalLogger rootLoggerName (setLevel level)
  where
    openLog path = openFile path AppendMode
    isoDatetime = formatTime defaultTimeLocale "%F-%X"