module Game (
    startGame,
    stopGame,
    ServerList,
    Name,
    Host,
    ThreadId
) where

import Packets
import Network.Socket
import Control.Monad (forever)
import Control.Exception (finally)
import Control.Concurrent

--
-- Types
--
type Host = String
type Name = String
type ServerList = [(Name, ThreadId)]

-- | Creats an UDP broadcast socket (+ReuseAddr, +DontRoute)
udpSocket :: IO (Socket)
udpSocket = 
    do sock <- socket AF_INET Datagram 0
       setSocketOption sock Broadcast 1
       setSocketOption sock DontRoute 1
       return sock

-- | Broadcasts that there's a game every 5 seconds.
broadcastGame :: Host -> Name -> IO ()
broadcastGame host name = withSocketsDo $ 
    udpSocket >>= \sock ->
    let announceGame = forever (do broadcast (w3GS_LAN_GAMEINFO name)
                                   threadDelay 5000000)
        destroyGame  = broadcast w3GS_LAN_DECREATEGAME
        broadcast  p = inet_addr host >>= sendTo sock (packet p) . SockAddrInet 6112
    in announceGame `finally` destroyGame

-- | Spawns a broadcasting thread
startGame :: Name -> IO ThreadId
startGame = forkIO . broadcastGame "10.0.2.255"

stopGame :: ThreadId -> IO ()
stopGame = killThread