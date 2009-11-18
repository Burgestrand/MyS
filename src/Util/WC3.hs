{-|
    Utility functions for WC3 packets. To be moved to same location as protocol
    handling code.
-}
module Util.WC3 (
    encode, decode, encoder, decoder, mapChunks
) where

import Data.Bits
import Data.List
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Ext.Data.List (spliti)

--------------------------------------------------------------------------------

-- | Applies a function to a ByteString by repacking it.
apply :: ([Word8] -> [Word8]) -> ByteString -> ByteString
apply f = L.pack . f . L.unpack

-- | Apply encoder / decoder to the Word8s in a bytestring.
encode, decode :: ByteString -> ByteString
encode = apply (mapChunks encoder 7)
decode = apply (mapChunks decoder 8)

mapChunks :: ([a] -> [b]) -> Int -> [a] -> [b]
mapChunks f n = concatMap f . spliti n

-- | This does the actual encoding work. Read Packets.mdown for documentation.
encoder :: [Word8] -> [Word8]
encoder xs = mask : xs'
           where (mask, xs') = mapAccumL f 1 (zip [1..] xs)
                 f m (i, b)  = if even b
                               then (m, b + 1)
                               else (m .|. (2 ^ i), b)
-- | Actual decoding work.
decoder :: [Word8] -> [Word8]
decoder (m:xs) = map f (zip [1..] xs)
               where f (i, b) = if m .&. (2 ^ i) == 0
                                then b - 1
                                else b