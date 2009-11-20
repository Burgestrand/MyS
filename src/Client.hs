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
    stdout,
    sock
) where

import Common
import Control.Concurrent
import Ext.Data.List
import Network.Socket (Socket)
import qualified Network.Socket as Socket

data Client = Client {
        -- | Messages from anyone to Client
        stdin  :: Chat Messages,
        -- | Messages from Client to anyone
        stdout :: Chat Messages,
        -- | The clients’ (most likely connected) socket
        sock   :: Socket
    }

instance Eq Client where
    c == c' = sock c == sock c'

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
runClient client = do
    catchE (runReaderT client clientHandler) catcher
           `finally` Socket.sClose (sock client)
  where
    catcher :: IOException -> IO ()
    catcher e = sendMessageIO (stdout client) (Disconnect (show e))

-- | Client handler
--   
--   1. Reads from the client socket, decoding any message and sends it
--      to the server.
--   2. Reads messages from the server and sends them to the client.
clientHandler :: ClientM ()
clientHandler = do
    -- Server -> Client
    forkReader $ 
        forever (getMessage >>= process)
    
    -- Client -> Server
    forever $ do
        msg <- fmap (head . lines) recv
        putMessage $ fromMaybe (Packet msg) (readm msg)

process :: Messages -> ClientM ()
process (Packet m)  = send ("Packet: " ++ m ++ "\n") >> return ()
process (Command m) = send ("Command: " ++ m ++ "\n") >> return ()
process msg         = io . putStrLn $ "Client.process: unhandled message " ++ show msg

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
peek :: ClientM Messages
peek = asks stdin >>= io . peekMessageIO

-- | 'receiveMessage' in the Client monad
getMessage :: ClientM Messages
getMessage = asks stdin >>= io . receiveMessageIO

-- | 'sendMessage' in the Client monad
putMessage :: Messages -> ClientM ()
putMessage msg = do
    chan <- asks stdout
    io $ sendMessageIO chan msg