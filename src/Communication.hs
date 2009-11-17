module Communication (
    Message(..),
    Messages,
    
    new,
    receive, send, clone,
    
    newIO,
    receiveIO, sendIO, cloneIO,
    
    count, countIO,
    
    STM,
    atomically, retry, orElse
) where

-- Imports
------------------------------------------------------------------------------
import Ext.Control.Concurrent

-- Types
------------------------------------------------------------------------------
-- The message channel and number of messages waiting to be read
newtype Messages = Messages (TChan Message, TMVar Int)

instance Show Messages where
    show (Messages (msgs, count)) = "Messages"

data Message = Connect
             | Disconnect
             | Packet String -- Packet from Network.BattleNet.Packets
    deriving (Show)

-- Functions
------------------------------------------------------------------------------
new :: STM Messages
new = do
    chan  <- newTChan
    count <- newTMVar 0
    return $ Messages (chan, count)

count :: Messages -> STM Int
count (Messages a) = readTMVar (snd a)

receive :: Messages -> STM Message
receive (Messages (chan, count)) = do
    msg <- readTChan chan
    modifyTMVar count (1-)
    return msg

send :: Messages -> Message -> STM ()
send (Messages (chan, count)) msg = do
    writeTChan chan msg
    modifyTMVar count (1+)
    
clone :: Messages -> STM Messages
clone (Messages (chan, count)) = do
    chan' <- dupTChan chan
    return $ Messages (chan', count)

-- IO Counterparts
newIO      = atomically new
receiveIO  = atomically . receive
sendIO ch  = atomically . send ch
cloneIO    = atomically . clone
countIO    = atomically . count