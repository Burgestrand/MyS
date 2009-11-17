module Ext.Network.Socket (
    module Network.Socket,
    module Network.Socket.ByteString,
    inet_aton,
    inet_ntoa
) where

-- Exports
import Network.Socket hiding (inet_ntoa, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

-- Functions
import Data.Bits
import Data.List (intercalate)

import Control.Monad

-- *** UNSAFE ***
------------------------------------------------------------------------------
import System.IO.Unsafe (unsafePerformIO)
------------------------------------------------------------------------------

-- | A pure inet_aton (uses unsafePerformIO)
inet_aton :: String -> Maybe HostAddress
inet_aton s = unsafePerformIO $ (return . Just =<< inet_addr s)
                                `catch` (return . const Nothing)

-- | A pure inet_ntoa
inet_ntoa :: HostAddress -> String
inet_ntoa n = intercalate "." $ map (show . f) [0, 8, 16, 24]
            where f x = 0xFF .&. shiftR n x