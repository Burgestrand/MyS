import System.Environment (getArgs)
import Control.Exception (finally)
import System.IO (hClose, hSetBuffering, BufferMode(NoBuffering))
import Network
import Data.Word
import Data.Bits -- Bitwise manipulation
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

serialize :: Word32 -> Put -- accept name parameter, too!
serialize hcount = do
    putWord8 0xF7 -- W3GS
    putWord8 0x1E -- REQJOIN
    putWord16le 0x29 -- size
    putWord32le hcount -- host count
    putWord32le 0x00 -- Win32 GetTickCount
    putWord8 0x00 -- zeroes (password?)
    putWord16le 0x00
    putWord32le 0x00
    
    -- Name
    putWord8 0x4b
    putWord8 0x69
    putWord8 0x6d
    putWord8 0x00
    
    -- AF_INET
    putWord16le 0x01 -- af_inet
    putWord16le 0x00 -- ipv4 type
    putWord16le 0x00 -- port
    putWord32le 0x00 -- IPv4
    putWord64le 0x00 -- IPv6

-- | Fires up the refresher application.
main :: IO ()
main = withSocketsDo $ do
    let host = "127.0.0.1"
    let port = PortNumber 6119
    h <- connectTo host port
    hSetBuffering h (NoBuffering)
    (BL.hPut h . runPut $ serialize 0x3) `finally` hClose h