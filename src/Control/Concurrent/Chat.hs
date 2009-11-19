{-|
    'Communication' provides an abstraction layer on communication 
    between threads.
-}
module Control.Concurrent.Chat (
    -- * Types
    Chat,
    Message(..),
    
    -- * Functions
    
    -- ** STM
    newChat,
    cloneChat,
    countMessages,
    receiveMessage,
    peekMessage,
    sendMessage,
    
    -- ** IO
    -- | Same functions as above, but within the IO monad.
    newChatIO,
    cloneChatIO,
    receiveMessageIO,
    peekMessageIO,
    sendMessageIO,
    countMessagesIO,
    
    -- * Control.Monad.STM
    STM,
    atomically, retry, orElse
) where

import Ext.Control.Concurrent

-- | A 'Chat' is basically a FIFO channel of Messages
newtype Chat = Chat (TChan Message, TMVar Int)

-- | The various message types used for cross-thread communication
data Message = Packet String
             | Disconnect { reason :: String }
    deriving (Show, Eq)

-- | Creates a new Chat channel
newChat :: STM Chat
newChat = do
    chan  <- newTChan
    count <- newTMVar 0
    return $ Chat (chan, count)

-- | Counts the unread Chat in a Chat channel
countMessages :: Chat -> STM Int
countMessages (Chat a) = readTMVar (snd a)

-- | Reads a message from a Chat channel
receiveMessage :: Chat -> STM Message
receiveMessage (Chat (chan, count)) = do
    msg <- readTChan chan
    modifyTMVar count (1-)
    return msg

-- | Sends a message out onto the Chat channel
sendMessage :: Chat -> Message -> STM ()
sendMessage (Chat (chan, count)) msg = do
    writeTChan chan msg
    modifyTMVar count (1+)

-- | Clones a Chat channel. Data written to either is available on both
cloneChat :: Chat -> STM Chat
cloneChat (Chat (chan, count)) = do
    chan' <- dupTChan chan
    return $ Chat (chan', count)

-- | Peek’s in a Chat channel; returning the next item without removing it
peekMessage :: Chat -> STM Message
peekMessage (Chat (chan, count)) = do
    msg <- readTChan chan
    unGetTChan chan msg
    return msg

-- Convenience: types
newChatIO        :: IO Chat
cloneChatIO      :: Chat -> IO Chat
receiveMessageIO :: Chat -> IO Message
peekMessageIO    :: Chat -> IO Message
sendMessageIO    :: Chat -> Message -> IO ()
countMessagesIO  :: Chat -> IO Int

-- Convenience. functions
newChatIO        = atomically newChat
cloneChatIO      = atomically . cloneChat
receiveMessageIO = atomically . receiveMessage
peekMessageIO    = atomically . peekMessage
sendMessageIO ch = atomically . sendMessage ch
countMessagesIO  = atomically . countMessages