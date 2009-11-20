{-# LANGUAGE FlexibleContexts #-}
{-|
    'Common' exports a lot of functionality that is common to all 
    subparts of MyS.
-}
module Common (
    module Control.Concurrent.Chat,
    module Communication,
    module Configuration,
    module Ext.Data.Bool,
    module Data.Maybe,
    module Util.Log,
    module Ext.Text.Read,
    module Ext.Control.Monad,
    ioSTM,
    putStrLnM,
    forkReader,
    prompt, promptm
) where

import Ext.Control.Monad
import Control.Concurrent
import Control.Concurrent.Chat
import Communication
import Configuration
import Data.Maybe
import Util.Log
import Ext.Text.Read
import Ext.Data.Bool

import System.IO

-- | 'io' . 'atomically'
ioSTM :: (BaseM m IO) => STM a -> m a
ioSTM = io . atomically

-- | 'io' . 'putStrLn'
putStrLnM :: (BaseM m IO) => String -> m ()
putStrLnM = io . putStrLn

-- | Run a Reader Monad in a separate thread
forkReader :: (ReaderM m a, BaseM m IO) => ReaderT a IO () -> m ThreadId
forkReader reader = ask >>= io . forkIO . flip runReaderT reader

-- | Prompt the user with the given string prefix
prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine

promptm :: (Read a) => String -> a -> IO a
promptm str d = fmap (readDefault d) (prompt str)