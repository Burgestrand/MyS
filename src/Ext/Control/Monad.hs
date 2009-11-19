module Ext.Control.Monad (
    module MonadLib,
    whileM
) where

import MonadLib

-- | While the first argument holds true, evaluate the second
--
-- > whileM (getLine >>= return . (/= "y")) 
-- >        (print "x")
whileM :: (Monad m) => m Bool -> m a -> m ()
whileM condition action = do
    condition >>= run
  where
    run False = return ()
    run True  = action >> whileM condition action