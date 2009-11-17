{-
    This module contains code that announces an existing server
    to the network.
-}
module Announcer (startAnnouncer) where

-- Logging
import Util.Log

-- Networking
import Ext.Network.Socket

-- Communication
import Ext.Data.ByteString
import qualified Data.ByteString.Lazy as Lazy
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

-- Concurrency
import Control.Concurrent (forkIO, threadDelay)

-- Flow control
import Control.Monad (forever)

-- Convenience
import Data.Maybe (fromMaybe)
import Prelude hiding (error)

-- | Main entry point
------------------------------------------------------------------------------
startAnnouncer :: IO (TMVar Lazy.ByteString)
startAnnouncer = do
    debug "Announcer starting."
    var <- newEmptyTMVarIO
    forkIO $ announce var `catch` (error . show)
    return var
    
-- | Announcer
announce :: TMVar Lazy.ByteString -> IO ()
announce var = do
    -- Broadcast socket
    sock <- socket AF_INET Datagram 17
    setSocketOption sock Broadcast 1
    
    debug "Announcer socket set up."
    
    -- Read and send packet
    forever $ do
        packet <- atomically (readTMVar var)
        broadcast sock (toStrict packet)
        debug "Announced."
        threadDelay 5000000
  where
    broadcast sock msg = sendTo sock msg (SockAddrInet 6112 (fromMaybe 0 $ inet_aton "127.0.0.1"))

debug :: String -> IO ()
debug = debugM "My.Announcer"

error :: String -> IO ()
error = errorM "My.Announcer"