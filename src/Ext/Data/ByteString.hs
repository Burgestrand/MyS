module Ext.Data.ByteString (    
    toLazy,
    toStrict,
    fromString,
    toString
) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as C (pack, unpack)

-- | Turns a strict ByteString to a lazy ByteString
toLazy :: Strict.ByteString -> Lazy.ByteString
toLazy x = Lazy.fromChunks [x]

-- | Turns a lazy ByteString into a strict ByteString
toStrict :: Lazy.ByteString -> Strict.ByteString
toStrict x = Strict.concat . Lazy.toChunks $ x

-- | Turns a String into a lazy ByteString
fromString :: String -> Lazy.ByteString
fromString = C.pack

-- | Turns a lazy ByteString into a String
toString :: Lazy.ByteString -> String
toString = C.unpack