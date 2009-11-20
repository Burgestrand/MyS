module Ext.Control.Monad (
    module MonadLib,
    module Control.Exception,
    whileM,
    io,
    
    -- * To counter conflicting imports do some things explicitly
    tryM, handleM, mapExceptionM,
    tryE, handleE, mapExceptionE,
    catchE
) where

import MonadLib hiding (try, handle, mapException)
import Control.Exception hiding (try, handle, mapException)

import qualified MonadLib
import qualified Control.Exception

-- | While the first argument holds true, evaluate the second
--
-- > whileM (getLine >>= return . (/= "y")) 
-- >        (print "x")
whileM :: (Monad m) => m Bool -> m a -> m ()
whileM condition action = condition >>= run
  where
    run False = return ()
    run True  = action >> whileM condition action

-- | Alias for "MonadLib#inBase"
io :: (BaseM m n) => n a -> m a
io = inBase

-- Explicitness
------------------------------------------------------------------------

-- | "MonadLib#try"
tryM :: (RunExceptionM m i) => m a -> m (Either i a)
tryM = MonadLib.try

-- | "MonadLib#handle"
handleM :: (RunExceptionM m x) => m a -> (x -> m a) -> m a
handleM = MonadLib.handle

-- | "MonadLib#mapException"
mapExceptionM :: (RunExceptionM m x) => (x -> x) -> m a -> m a
mapExceptionM = MonadLib.mapException

-- | "Control.Exception#handle"
handleE :: (Exception e) => (e -> IO a) -> IO a -> IO a
handleE = Control.Exception.handle

-- | "Control.Exception#try"
tryE :: (Exception e) => IO a -> IO (Either e a)
tryE = Control.Exception.try

-- | "Control.Exception#mapException"
mapExceptionE :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapExceptionE = Control.Exception.mapException

-- | "Control.Exception#catch"
catchE :: (Exception e) => IO a -> (e -> IO a) -> IO a
catchE = Control.Exception.catch