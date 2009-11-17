module Configuration where

-- Imports
------------------------------------------------------------------------------

-- Standard
import System.IO.Unsafe
import System.Environment (getArgs)

-- Local
import Ext.Text.Read
import Util.Ternary

-- UNSAFE
------------------------------------------------------------------------------
{-# NOINLINE pureArgs #-}
pureArgs :: [String]
pureArgs = unsafePerformIO getArgs

-- Configuration!
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