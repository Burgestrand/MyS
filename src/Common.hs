{-# LANGUAGE FlexibleContexts #-}
{-|
    'Common' exports a lot of functionality that is common to all 
    subparts of MyS.
-}
module Common (
    module Control.Concurrent.Chat,
    module Configuration,
    module Ext.Data.Bool,
    module Ext.Control.Monad,
    module Data.Maybe,
    module Util.Log,
    ioSTM,
    putStrLnM,
    forkReader
) where

import Control.Concurrent
import Control.Concurrent.Chat
import Configuration

import Data.Maybe
import Util.Log

import Ext.Data.Bool
import Ext.Control.Monad

-- | 'io' . 'atomically'
ioSTM :: (BaseM m IO) => STM a -> m a
ioSTM = io . atomically

-- | 'io' . 'putStrLn'
putStrLnM :: (BaseM m IO) => String -> m ()
putStrLnM = io . putStrLn

-- | Run a Reader Monad in a separate thread
forkReader :: (ReaderM m a, BaseM m IO) => ReaderT a IO () -> m ThreadId
forkReader reader = ask >>= io . forkIO . flip runReaderT reader