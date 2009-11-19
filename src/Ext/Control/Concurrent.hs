{-|
    A re-export of concurrency modules for easier access, and some of
    my own additions.
-}
module Ext.Control.Concurrent (
    module Control.Concurrent,
    module Control.Concurrent.STM,
    writeTMVar,
    modifyTMVar,
    tselect
) where

import Control.Concurrent
import Control.Concurrent.STM

-- | Modify the contents of a TMVar 
modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = do
    a <- takeTMVar var
    writeTMVar var (f a)

-- | Write a value to a TMVar. Overwrite if already full.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a = do
    m <- tryTakeTMVar var
    putTMVar var a

-- | Return the value of the first STM action that doesn’t retry
tselect :: [STM a] -> STM a
tselect = foldl orElse retry