module Ext.Data.ByteString (
    toLazy,
    toStrict,
    fromString,
    toString
) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as C (pack, unpack)

-- | Turns a Strict bytestring to a Lazy bytestring
toLazy :: Strict.ByteString -> Lazy.ByteString
toLazy x = Lazy.fromChunks [x]

-- | Turns a Lazy bytestring into a Strict bytestring
toStrict :: Lazy.ByteString -> Strict.ByteString
toStrict x = Strict.concat . Lazy.toChunks $ x

-- | Converting to and from lazy bytestrings.
fromString :: String -> Lazy.ByteString
fromString = C.pack

toString :: Lazy.ByteString -> String
toString = C.unpack