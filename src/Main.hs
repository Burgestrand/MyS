{-
  This code will require refactoring.

  Currently this is a chat server.
-}
module Main where

-- | Imports
------------------------------------------------------------------------------

-- Program components
import Announcer

-- Flow control
import Control.Monad

-- Messaging & Concurrency
import Ext.Control.Concurrent

-- Communication
import Ext.Data.ByteString
import qualified Data.ByteString.Lazy as Lazy

-- Misc
import Util.Log

-- | Main program
------------------------------------------------------------------------------
main :: IO ()
main = do
    initLogger DEBUG
    announce <- startAnnouncer
    broadcast announce

broadcast :: TMVar Lazy.ByteString -> IO ()
broadcast tmvar = forever $ do
    debugM "My" "Listening for input."
    msg <- getLine
    atomically $ writeTMVar tmvar (fromString msg)
    debugM "My" $ "Written " ++ show msg ++ " to MVar."
  where
    announce = atomically . writeTMVar tmvar