module Client where

-- Imports
------------------------------------------------------------------------------
import Common

-- Threads
import Control.Concurrent

-- Transformers
import MonadLib

-- Networking
import Ext.Network.Socket

-- Types
------------------------------------------------------------------------------

-- | (to client, from client)
data Client = Client {
        stdin  :: Messages,
        stdout :: Messages,
        sock   :: Socket
    } deriving (Show)

-- Functions
------------------------------------------------------------------------------
mkClient :: Socket -> IO Client
mkClient sock = do
    stdin  <- newIO
    stdout <- newIO
    return Client { stdin  = stdin
                  , stdout = stdout
                  , sock   = sock }

-- | Handle a socket.
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

-- | Read the clients’ input channel.
getMessage :: ClientM Message
getMessage = do
    chan <- asks stdin
    inBase $ receiveIO chan

-- | Write to the clients’ output channel.
putMessage :: Message -> ClientM ()
putMessage msg = do
    chan <- asks stdout
    inBase $ atomically $ do
        n <- count chan
        when (n > 100) retry -- max message queue
        Common.send chan msg