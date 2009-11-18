{-|
    This module provides the full client interface.
    
    When a new connection is established the connected socket is passed
    to the 'handle' function. After that communication to the client
    is handled through the "Communication" module.
-}
module Client (
    Client,
    handle
) where

import Common
import Control.Concurrent
import MonadLib hiding (handle)
import Ext.Network.Socket

data Client = Client {
        -- | Messages from anyone to Client
        stdin  :: Messages,
        -- | Messages from Client to anyone
        stdout :: Messages,
        -- | The clients’ (most likely connected) socket
        sock   :: Socket
    } deriving (Show)

-- | Creates a new Client with the specified socket.
mkClient :: Socket -> IO Client
mkClient sock = do
    stdin  <- newIO
    stdout <- newIO
    return Client { stdin  = stdin
                  , stdout = stdout
                  , sock   = sock }

-- | Client handler entry point; returns immediately.
handle :: Socket -> IO (Client, ThreadId)
handle sock = do
    client <- mkClient sock
    thread <- forkIO (runClient client)
    return (client, thread)

type ClientM = ReaderT Client IO
runClient :: Client -> IO ()
runClient = flip runReaderT clientHandler

-- | Client handler
clientHandler :: ClientM ()
clientHandler = do
    whileM continue process
    inBase $ print "Bye bye"
  where
    continue = peekMessage >>= return . (/= Disconnect)
    process  = do
        msg <- getMessage
        inBase $ print msg

-- | 'peekIO' in the Client monad
peekMessage :: ClientM Message
peekMessage = asks stdin >>= inBase . peekIO

-- | 'receiveIO' in the Client monad
getMessage :: ClientM Message
getMessage = asks stdin >>= inBase . receiveIO

-- | 'sendIO' in the Client monad
putMessage :: Message -> ClientM ()
putMessage msg = do
    chan <- asks stdout
    inBase $ sendIO chan msg