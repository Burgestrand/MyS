{-|
    'Communication' provides an abstraction layer on communication 
    between threads.
-}
module Communication (
    -- * Types
    Message(..),
    Messages,
    
    -- * Functions
    
    -- ** STM
    new,
    receive,
    peek,
    send,
    clone,
    count,
    
    -- ** IO
    -- | Same functions as above, but within the IO monad.
    newIO,
    receiveIO,
    peekIO,
    sendIO,
    cloneIO,
    countIO,
    
    -- * Control.Monad.STM
    STM,
    atomically, retry, orElse
) where

import Ext.Control.Concurrent

newtype Messages = Messages (TChan Message, TMVar Int)

-- | Required for debugging. TODO: REMOVE ME.
instance Show Messages where
    show (Messages (msgs, count)) = "Messages"

-- | The various message types used for cross-thread communication
data Message = Connect
             | Disconnect
             | Packet String -- Packet from Network.BattleNet.Packets
    deriving (Show, Eq)

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

-- | Peek’s in a message channel; returning the next item without removing it
peek :: Messages -> STM Message
peek (Messages (chan, count)) = do
    msg <- readTChan chan
    unGetTChan chan msg
    return msg

-- Convenience: types
newIO     :: IO Messages
receiveIO :: Messages -> IO Message
peekIO    :: Messages -> IO Message
sendIO    :: Messages -> Message -> IO ()
cloneIO   :: Messages -> IO Messages
countIO   :: Messages -> IO Int

-- Convenience. functions
newIO      = atomically new
receiveIO  = atomically . receive
peekIO     = atomically . peek
sendIO ch  = atomically . send ch
cloneIO    = atomically . clone
countIO    = atomically . count