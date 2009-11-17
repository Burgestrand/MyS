-- | This module is for nothing but convenience.
module Ext.Data.Binary (
    Binary (..),
    Get,
    runGet,
    Put,
    runPut,
    
    encode,
    decode,
    
    skip,
    
    putWord8,
    getWord8,
    
    putWord16le,
    getWord16le,
    
    putWord32le,
    getWord32le,
    
    putWord64le,
    getWord64le,
    
    putLazyByteString,
    getLazyByteString,
    
    putLazyByteStringNul,
    getLazyByteStringNul
) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy (ByteString)

-- | Puts a Lazy bytestring and puts a NULL byte on the end.
putLazyByteStringNul :: ByteString -> Put
putLazyByteStringNul b = putLazyByteString b >> putWord8 0