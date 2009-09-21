module Packets (
    Packet,
    packet,
    unpacket,
    w3GS_LAN_CREATEGAME,
    w3GS_LAN_REFRESHGAME,
    w3GS_LAN_DECREATEGAME,
    w3GS_LAN_SEARCHGAME,
    w3GS_LAN_GAMEINFO
) where

import Data.Bits
import Data.List
import Data.String
import Data.Word
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

-- Packet thingy
type Packet = C.ByteString

-- | Takes a bytestring and assigns the length.
assignLength :: Packet -> Packet
assignLength str = runPut $ do
    putLazyByteString $ BL.take 2 str -- Remove game + packet signature
    putWord16le       $ fromIntegral $ BL.length str -- Assign length
    putLazyByteString $ BL.drop 4 str -- Rest of packet

-- | W3XP, v24, game no #01
w3GS_LAN_CREATEGAME :: Packet
w3GS_LAN_CREATEGAME = assignLength $ runPut $ do
    putWord8          0xF7 -- W3GS
    putWord8          0x31 -- LAN_CREATEGAME
    putWord16le       0x00 -- Length assigned later
    putLazyByteString $ fromString (reverse "W3XP") -- Game signature
    putWord32le       0x18 -- Game version
    putWord32le       0x01 -- Creation counter

-- | Takes the amount of players in game and open game slots and makes a packet of it
w3GS_LAN_REFRESHGAME :: (Integral a) => a -> a -> Packet
w3GS_LAN_REFRESHGAME players slots = assignLength $ runPut $ do
    putWord8    0xF7
    putWord8    0x32
    putWord16le 0x00                   -- Length assigned later
    putWord32le 0x01                   -- Creation counter
    putWord32le $ fromIntegral players -- Players currently in game
    putWord32le $ fromIntegral slots   -- Amount of open slots
    
-- | Client sends this to find games.
w3GS_LAN_SEARCHGAME :: Packet
w3GS_LAN_SEARCHGAME = assignLength $ runPut $ do
    putWord8          0xF7
    putWord8          0x2F
    putWord16le       0x00                          -- Length assigned later
    putLazyByteString $ fromString (reverse "W3XP") -- Game signature
    putWord32le       0x18
    putWord32le       0

-- | Decreates game; always host count 1
w3GS_LAN_DECREATEGAME :: Packet
w3GS_LAN_DECREATEGAME = assignLength $ runPut $
    do putWord8    0xF7
       putWord8    0x33
       putWord16le 0x00
       putWord32le 0x01

-- | W3GS_LAN_GAMEINFO; host count 1
w3GS_LAN_GAMEINFO :: String -> Packet
w3GS_LAN_GAMEINFO gameName = assignLength $ runPut $ 
    do putWord8          0xF7
       putWord8          0x30
       putWord16le       0x00                     -- Length assigned later
       putLazyByteString $ fromString (reverse "W3XP") -- Game signature
       putWord32le       0x18                     -- Version
       putWord32le       0x01                     -- Host counter
       putWord32le       0x00                     -- UNKNOWN
       putLazyByteString $ fromString gameName    
       putWord8          0x00                     -- Null terminator for game name
       putWord8          0x00                     -- Start of encoded data
       putLazyByteString $ BL.pack $ concat $ map encodeGameInfo $ split 7 $ BL.unpack gameInfo
       putWord8          0x00
       putWord32le       0x0A                     -- Total game slots (10)
       putWord32le       0x01                     -- Game type
       putWord32le       0x01                     -- Unkown
       putWord32le       0x0B                     -- Open (available) slots
       putWord32le       0x01                     -- Game uptime
       putWord16le       6112                     -- Host port

-- | Encodes an amount of bytes as if they were 
encodeGameInfo :: [Word8] -> [Word8]
encodeGameInfo xs = mask : encoded
    where (mask, encoded) = mapAccumL encoder 1 (zip [1..] xs)
          encoder mask (i, x) = if even x 
                                then (mask, x + 1)
                                else (mask .|. (2 ^ i), x)

-- | Decodes a group of bytes
decodeGameInfo :: [Word8] -> [Word8]
decodeGameInfo (mask:xs) = [ real x i | (i, x) <- zip [1..] xs ]
    where real x i = if mask .&. (2 ^ i) == 0
                     then x - 1
                     else x

-- | Make groups of n’s of xs
split :: Int -> [a] -> [[a]]
split n = unfoldr f
        where f xs | null xs   = Nothing
                   | otherwise = Just (splitAt n xs)

-- | Takes a ByteString and turns it into a String for sending
packet :: Packet -> String
packet = C.unpack

-- | Takes a String and turns it into a ByteString
unpacket :: String -> Packet
unpacket = C.pack

-- | gameInfo (not encoded)
gameInfo :: Packet
gameInfo = runPut $ do
    putWord8          0x01 -- Fast game speed
    putWord8          0x48 -- Observers On
    putWord8          0x06 -- Fixed teams
    putWord8          0x00 -- Units, nothing
    putWord8          0x00 -- zero
    putWord16le       0x00 -- map width
    putWord16le       0x00 -- map height
    putWord32le       0x00 -- map CRC32
    putLazyByteString $ fromString "Maps\\Download\\DotA Allstars v6.62b.w3x"
    putWord8          0x00 -- null terminator
    putLazyByteString $ fromString "Kim Burgestrand"
    putWord8          0x00 -- Null terminator
    putWord8          0x00 -- end encoded data

main :: IO ()
main = putStr "Hi"