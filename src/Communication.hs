{-|
    Communication module provides a simplified interface for communication 
    between threads. Internally it uses TChannels coupled with a counter.
-}
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

import Ext.Control.Concurrent

-- | The message channel and number of messages waiting to be read
newtype Messages = Messages (TChan Message, TMVar Int)

-- | Required for debugging. TODO: REMOVE ME.
instance Show Messages where
    show (Messages (msgs, count)) = "Messages"

data Message = Connect
             | Disconnect
             | Packet String -- Packet from Network.BattleNet.Packets
    deriving (Show)


-- | Creates a new Message channel.
new :: STM Messages
new = do
    chan  <- newTChan
    count <- newTMVar 0
    return $ Messages (chan, count)

-- | Counts the unread messages in a Message channel.
count :: Messages -> STM Int
count (Messages a) = readTMVar (snd a)

-- | Reads a message from a Message channel.
receive :: Messages -> STM Message
receive (Messages (chan, count)) = do
    msg <- readTChan chan
    modifyTMVar count (1-)
    return msg

-- | Sends a message out onto the Message channel.
send :: Messages -> Message -> STM ()
send (Messages (chan, count)) msg = do
    writeTChan chan msg
    modifyTMVar count (1+)

-- | Clones a Message channel. Data written to either is available on both.
clone :: Messages -> STM Messages
clone (Messages (chan, count)) = do
    chan' <- dupTChan chan
    return $ Messages (chan', count)

-- Convenience
newIO      = atomically new
receiveIO  = atomically . receive
sendIO ch  = atomically . send ch
cloneIO    = atomically . clone
countIO    = atomically . count