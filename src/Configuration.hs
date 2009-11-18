{-|
    A common interface to the applications’ Configuration. Could be either a
    textfile, database or something similar.
-}
module Configuration where

import System.IO.Unsafe
import System.Environment (getArgs)
import Ext.Text.Read
import Ext.Data.Bool

-- UNSAFE
------------------------------------------------------------------------------
{-# NOINLINE pureArgs #-}
pureArgs :: [String]
pureArgs = unsafePerformIO getArgs
------------------------------------------------------------------------------

data Configuration = Configuration {
                         host :: String,
                         port :: Integer
                     }
                     deriving (Show)

-- | Parse program arguments into config (unsafeish)
cfgArgs :: Configuration
cfgArgs = Configuration 
    {
        host = (not . null) ?? head args $ "127.0.0.1",
        port = readDefault 6112 (args !! 1)
    }
  where
    args = pureArgs ++ repeat ""