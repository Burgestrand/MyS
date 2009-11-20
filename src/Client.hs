{-|
    This module provides the full client interface.
    
    When a new connection is established the connected socket is passed
    to the 'handle' function. After that communication to the client
    is handled through the "Communication" module.
-}
module Client (
    Client,
    handle,
    stdin,
    stdout
) where

import Common hiding (handle)
import Control.Concurrent
import Ext.Data.List
import Network.Socket (Socket)
import Control.Exception (finally)
import qualified Network.Socket as Socket

data Client = Client {
        -- | Messages from anyone to Client
        stdin  :: Chat,
        -- | Messages from Client to anyone
        stdout :: Chat,
        -- | The clients’ (most likely connected) socket
        sock   :: Socket
    }

-- | Creates a new Client with the specified socket.
mkClient :: Socket -> IO Client
mkClient sock = do
    stdin  <- newChatIO
    stdout <- newChatIO
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

-- XXX: Make something smarter with the error handling
runClient :: Client -> IO ()
runClient client = 
    finally (runReaderT client clientHandler)
            $ do Socket.sClose (sock client)
                 sendMessageIO (stdout client) (Disconnect "Closed connection.")

-- | Client handler
--   
--   1. Reads from the client socket, decoding any message and sends it
--      to the server.
--   2. Reads messages from the server and sends them to the client.
clientHandler :: ClientM ()
clientHandler = do
    -- Client -> Server
    forkReader . forever $ do
        str <- recv
        putMessage (Packet str)
    -- Server -> Client
    whileM (peek >>= continue)
           (getMessage >>= process)
  where
    continue :: Message -> ClientM Bool
    continue (Disconnect _) = return False
    continue _              = return True

process :: Message -> ClientM ()
process (Packet m) = send m >> return ()

-- * Messaging
------------------------------------------------------------------------

-- | "Network.Socket#recv" in the Client Monad
recv :: ClientM String
recv = do
    sock <- asks sock
    io $ Socket.recv sock 65535

-- | "Network.Socket#send" in the Client Monad
send :: String -> ClientM Int
send msg = do
    sock <- asks sock
    io $ Socket.send sock msg

-- | 'peekMessage' in the Client Monad
peek :: ClientM Message
peek = asks stdin >>= io . peekMessageIO

-- | 'receiveMessage' in the Client monad
getMessage :: ClientM Message
getMessage = asks stdin >>= io . receiveMessageIO

-- | 'sendMessage' in the Client monad
putMessage :: Message -> ClientM ()
putMessage msg = do
    chan <- asks stdout
    io $ sendMessageIO chan msg