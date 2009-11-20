{-|
    'Communication' provides an abstraction layer on communication 
    between threads.
-}
module Control.Concurrent.Chat (
    -- * Types
    Chat,
    
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
newtype Chat a = Chat (TChan a, TMVar Int)

-- | Creates a new Chat channel
newChat :: STM (Chat a)
newChat = do
    chan  <- newTChan
    count <- newTMVar 0
    return $ Chat (chan, count)

-- | Counts the unread Chat in a Chat channel
countMessages :: Chat a -> STM Int
countMessages (Chat a) = readTMVar (snd a)

-- | Reads a message from a Chat channel
receiveMessage :: Chat a -> STM a
receiveMessage (Chat (chan, count)) = do
    msg <- readTChan chan
    modifyTMVar count (1-)
    return msg

-- | Sends a message out onto the Chat channel
sendMessage :: Chat a -> a -> STM ()
sendMessage (Chat (chan, count)) msg = do
    writeTChan chan msg
    modifyTMVar count (1+)

-- | Clones a Chat channel. Data written to either is available on both
cloneChat :: Chat a -> STM (Chat a)
cloneChat (Chat (chan, count)) = do
    chan' <- dupTChan chan
    return $ Chat (chan', count)

-- | Peek’s in a Chat channel; returning the next item without removing it
peekMessage :: Chat a -> STM a
peekMessage (Chat (chan, count)) = do
    msg <- readTChan chan
    unGetTChan chan msg
    return msg

-- Convenience: types
newChatIO        :: IO (Chat a)
cloneChatIO      :: Chat a -> IO (Chat a)
receiveMessageIO :: Chat a -> IO a
peekMessageIO    :: Chat a -> IO a
sendMessageIO    :: Chat a -> a -> IO ()
countMessagesIO  :: Chat a -> IO Int

-- Convenience. functions
newChatIO        = atomically newChat
cloneChatIO      = atomically . cloneChat
receiveMessageIO = atomically . receiveMessage
peekMessageIO    = atomically . peekMessage
sendMessageIO ch = atomically . sendMessage ch
countMessagesIO  = atomically . countMessages