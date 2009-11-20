module Server where

import Common
import Client (Client)
import qualified Client
import Control.Exception (block)
import Ext.Data.List
import Ext.Control.Concurrent

import Network.Socket
import qualified Network.Socket as Socket

data Server = Server {
        -- | List of clients (needs cross-thread accessibility)
        clients :: TMVar [(ThreadId, Client)] -- XXX: replace with Map
    }

mkServer :: IO Server
mkServer = do
    clients <- newTMVarIO []
    return Server { clients = clients }

-- | Start a server and accept clients
main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 6112 0)
    listen sock 10
    runServer sock

-- | Serve a socket: might never ever return
runServer :: Socket -> IO ()
runServer sock = do
    s <- mkServer
    runReaderT s (server sock)
    return ()

type ServerM = ReaderT Server IO
server :: Socket -> ServerM ()
server sock = do
    forkReader $ runAcceptLoop sock
    forkReader runReader
    runInteractive

runAcceptLoop :: Socket -> ServerM ()
runAcceptLoop sock = forever $ do
    (sock', addr)      <- io $ Socket.accept sock
    (client, threadid) <- io $ Client.handle sock'
    addClient (threadid, client)

-- | Handles input from clients: ie. prints it
runReader :: ServerM ()
runReader = forever $ do
    clients <- asks clients
    msg <- ioSTM $ do --join
        cs <- readTMVar clients
        case cs of
             [] -> retry
             xs -> tselect
                 . map receiveMessage
                 . map Client.stdout
                 . map snd
                 $ xs
    sendAll msg

-- | Handles input from the user
runInteractive :: ServerM ()
runInteractive = forever $ do
    msg <- io $ prompt "server$> "
    sendAll $ maybe (Command msg) id (readm msg)

-- | Add a client to the list of clients
addClient :: (ThreadId, Client) -> ServerM ()
addClient client = asks clients >>= ioSTM . flip modifyTMVar (client:)

-- | Send a message to the specified client
sendOne :: Client -> Messages -> ServerM ()
sendOne c = io . sendMessageIO (Client.stdin c)

-- | Broadcast a message to all clients
sendAll :: Messages -> ServerM ()
sendAll msg = clientList >>= mapM_ (flip sendOne msg)

-- | Receive the first available message
getMessage :: ServerM Messages
getMessage = ioSTM 
           . tselect
           . map receiveMessage
           . map Client.stdout
           =<< clientList

-- | Retrieves sum of unread messages
getUnread :: [Chat Messages] -> STM Int
getUnread = foldl orElse retry
          . map countMessages

-- | Retrieve the list of clients
---  XXX: Make an STM version of this that uses retry if the list is empty
clientList :: ServerM [Client]
clientList = fmap (map snd) 
          . ioSTM 
          . readTMVar
          =<< asks clients