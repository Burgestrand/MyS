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
    runInteractive -- handle Ctrl-C

runAcceptLoop :: Socket -> ServerM ()
runAcceptLoop sock = forever $ do
    (sock', addr)      <- io $ Socket.accept sock
    (client, threadid) <- io $ Client.handle sock'
    addClient (threadid, client)

-- | Handles input from clients: ie. prints it
runReader :: ServerM ()
runReader = forever $ do
    clients <- asks clients
    (msg, thread) <- ioSTM $ do --join
        cs <- readTMVar clients
        case cs of
             [] -> retry
             _  -> tselect
                 . map (\(thread, client) -> do msg <- receiveMessage (Client.stdout client)
                                                return (msg, thread))
                 $ cs
    case msg of
         (Disconnect m) -> nukeThread thread
         _              -> process msg
    process msg
  where
    process (Packet m)  = sendAll (Packet m)
    process msg         = io $ print msg

-- | Handles input from the user
runInteractive :: ServerM ()
runInteractive = forever $ do
    msg <- io $ prompt "server$> "
    sendAll $ maybe (Command msg) id (readm msg)

-- | Add a client to the list of clients
addClient :: (ThreadId, Client) -> ServerM ()
addClient client = asks clients >>= ioSTM . flip modifyTMVar (client:)

-- | Nukes a client by:
--   1. Removing the client from the list of clients
--   2. Closing the clients’ socket
--   3. Killing the clients’ thread
nukeThread :: ThreadId -> ServerM ()
nukeThread thread = do
    clients <- asks clients
    cs <- ioSTM $ do
        cs <- readTMVar clients
        let cs_alive = filter ((/= thread) . fst) cs
        writeTMVar clients cs_alive
        return $ filter ((== thread) . fst) cs
    mapM_ (io . sClose . Client.sock . snd) cs
    mapM_ (io . killThread . fst) cs

-- | Send a message to the specified client
sendOne :: Client -> Messages -> ServerM ()
sendOne c = io . sendMessageIO (Client.stdin c)

-- | Send a message to everybody EXCEPT the specified client
sendAllBut :: Client -> Messages -> ServerM ()
sendAllBut c msg = mapM_ (flip sendOne msg) 
                 . filter (/= c)
                 =<< clientList

-- | Broadcast a message to all clients
sendAll :: Messages -> ServerM ()
sendAll msg = mapM_ (flip sendOne msg)
            =<< clientList

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