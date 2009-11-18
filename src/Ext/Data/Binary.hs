-- | This module is for nothing but convenience.
module Ext.Data.Binary (
    module Data.Binary,
    module Data.Binary.Get,
    module Data.Binary.Put,
    
    putLazyByteStringNul
) where

import Data.Binary hiding (Get, Put, getWord8, putWord8)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)

-- | Puts a lazy bytestring and appends a null-byte
putLazyByteStringNul :: ByteString -> Put
putLazyByteStringNul b = putLazyByteString b >> putWord8 0