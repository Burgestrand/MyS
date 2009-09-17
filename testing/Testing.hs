module Testing (main) where

import Packets

import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.Socket

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

{-
    What I need to do:
        Start a server that does the following:
            1. UDP broadcasts game create
            2. UDP broadcasts game refresh every 5 seconds
            3. Accepts UDP packets from a client that requests game info
            4. Responds with gameinfo
-}

-- | Creats an UDP broadcast socket (+ReuseAddr, +DontRoute)
udpSocket :: IO (Socket)
udpSocket = 
    do sock <- socket AF_INET Datagram 0
       setSocketOption sock Broadcast 1
       setSocketOption sock DontRoute 1
       return sock

-- | Broadcasts that there's a game every 5 seconds.
broadcastGame :: String -> IO ()
broadcastGame host = withSocketsDo $ 
    udpSocket >>= \sock ->
    let announceGame = forever (do broadcast (w3GS_LAN_GAMEINFO "Puffling")
                                   threadDelay 5000000)
        destroyGame  = broadcast w3GS_LAN_DECREATEGAME
        broadcast  p = inet_addr host >>= \host
                     -> sendTo sock (packet p) (SockAddrInet 6112 host) 
    in announceGame `finally` destroyGame
       
-- | Starts the application by firing up all the threads.
-- * The announcer that spams the local network about the existing game.
-- * The game info server that responds to UDP gameinfo requests.
-- * The main application, the actual server.
startApp :: IO ()
startApp = 
    do threads <- mapM forkIO (replicate 5 (broadcastGame "10.0.2.255"))
       forever yield `finally` mapM killThread threads

--
-- Fires up the application.
--
main :: IO ()
main =
    do args <- getArgs
       case args of
            [host, port] -> startApp
            _            -> startApp

-- | Prints a usage description.
usage :: IO ()
usage = do
    hPutStrLn stderr "Usage: host port"
    exitFailure