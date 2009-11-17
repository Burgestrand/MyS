module Ext.Control.Concurrent (
    module Control.Concurrent,
    
    -- STM
    module Control.Concurrent.STM,
    
    -- Extras
    writeTMVar,
    modifyTMVar
) where

-- Exports
import Control.Concurrent
import Control.Concurrent.STM

-- | Modify the contents of a TMVar.
modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = do
    a <- takeTMVar var
    writeTMVar var (f a)

-- | Write a value to a TMVar. Overwrite if already full.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a = do
    m <- tryTakeTMVar var
    putTMVar var a