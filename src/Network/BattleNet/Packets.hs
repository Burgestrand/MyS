{-
    This module contains a list of functions to create common packets!
-}
module Network.BattleNet.Packets (
    mkGameinfo,
    mkGameinfoPacket,
    
    encode,
    decode
) where

import Data.Binary (encode, decode)

-- Yay for protocols and stuff
import Network.BattleNet.Protocol
import Ext.Data.ByteString

-- | Packet creation
------------------------------------------------------------------------------

{-  | Takes the following arguments:
      - Game version
      - Host counter
      - Hosts port
      - Game name
      - Total game slots
      - Total open slots
      - Game uptime
      - Gameinfo type
-}
mkGameinfoPacket :: Int -> Int -> Int -> String -> Int -> Int -> Int -> Structure -> Packet
mkGameinfoPacket version counter port name slots openSlots uptime gameinfo = 
    mkPacket { protocol = W3GS LAN_GAMEINFO,
               payload  = [ mkW3XP  -- Game signature
                          , Long (fromIntegral version) -- Game version
                          , Long (fromIntegral counter)  -- Host counter
                          , Long 0  -- Unknown
                          , String (fromString name)
                          , Byte 0
                          , Struct gameinfo
                          , Byte 0
                          , Long (fromIntegral slots)  -- Total game slots
                          , Long 1   -- Game type
                          , Long 0   -- Unknown
                          , Long (fromIntegral openSlots)  -- Open slots
                          , Long (fromIntegral uptime)   -- Game uptime
                          , Short (fromIntegral port) -- Hosts' port
                          ] }