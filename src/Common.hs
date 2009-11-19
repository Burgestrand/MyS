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
    module Util.Log
) where

import Control.Concurrent.Chat
import Configuration

import Data.Maybe
import Util.Log

import Ext.Data.Bool
import Ext.Control.Monad

-- | Alias for "MonadLib#inBase"
io :: (BaseM m n) => n a -> m a
io = inBase

-- | 'io' . 'atomically'
ioSTM :: (BaseM m IO) => STM a -> m a
ioSTM = io . atomically

-- | 'io' . 'putStrLn'
putStrLnM :: (BaseM m IO) => String -> m ()
putStrLnM = io . putStrLn
