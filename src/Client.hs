{-|
    This module provides the full client interface. It handles receiving and
    responding to various packets, and talks to the server through channels.
    
    Main entry point is the 'handle' function.
-}
module Client (
    Client,
    handle
) where

import Common
import Control.Concurrent
import MonadLib
import Ext.Network.Socket

-- Types
------------------------------------------------------------------------------

data Client = Client {
        -- | Messages from anyone to Client
        stdin  :: Messages
        -- | Messages from Client to anyone
        stdout :: Messages,
        -- | The clients’ (most likely connected) socket
        sock   :: Socket
    } deriving (Show)

-- Functions
------------------------------------------------------------------------------

-- | Creates a new Client with the specified socket.
mkClient :: Socket -> IO Client
mkClient sock = do
    stdin  <- newIO
    stdout <- newIO
    return Client { stdin  = stdin
                  , stdout = stdout
                  , sock   = sock }

-- | Handle a socket.
--   Spawns a new thread, running the client in it. The function itself returns
--   immediately.
handle :: Socket -> IO (Client, ThreadId)
handle sock = do
    client <- mkClient sock
    thread <- forkIO (runClient client)
    return (client, thread)

-- Client Handler
------------------------------------------------------------------------------
type ClientM = ReaderT Client IO
runClient :: Client -> IO ()
runClient = flip runReaderT clientHandler

-- | Client handler.
clientHandler :: ClientM ()
clientHandler = do
    msg <- getMessage
    inBase (print msg)
    return ()

-- Client Monad utility
------------------------------------------------------------------------------

-- | Read the clients’ input channel. Waits if there is no data available.
getMessage :: ClientM Message
getMessage = do
    chan <- asks stdin
    inBase $ receiveIO chan

-- | Write to the clients’ output channel. Waits if there’s 100+ messages in
--   in the buffer.
putMessage :: Message -> ClientM ()
putMessage msg = do
    chan <- asks stdout
    inBase $ atomically $ do
        n <- count chan
        when (n > 100) retry -- max message queue
        Common.send chan msg