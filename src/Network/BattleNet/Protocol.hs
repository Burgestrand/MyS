module Network.BattleNet.Protocol where

-- | Standard library
import Data.Maybe (fromJust, fromMaybe)
import Data.Word

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy

-- | Extra libraries
import Ext.Data.Binary
import Ext.Data.ByteString
import Ext.Network.Socket (inet_ntoa, inet_aton, HostAddress)

-- | My own libraries
import Data.Table
import qualified Util.WC3 as Util

--------------------------------------------------------------------------------

data Packet = Packet { protocol :: Protocol
                     , size     :: Word16
                     , payload  :: [Data] -- Sequence instead?
                     }
              deriving (Show)

instance Binary Packet where
    put packet = do
        put (protocol packet)
        let load = runPut (mapM_ put (payload packet))
        putWord16le (size' load)
        putLazyByteString load
      where
        size' p = 4 + (fromIntegral . Lazy.length) p

    get = do
        proto <- get :: Get Protocol
        size <- getWord16le
        return Packet { protocol = proto,
                        size     = size,
                        payload  = [] }

--------------------------------------------------------------------------------

data Protocol = W3GS ID
              | BNET ID
                deriving (Show, Read)

instance Binary Protocol where
    put p@(W3GS id) = do
        put (0xF7 :: Word8)
        put id

    get = do
        proto <- get :: Get Word8
        id    <- get :: Get ID
        case proto of
          0xF7 -> return $ W3GS id
          _    -> error  $ "unknown protocol: " ++ show proto ++ "::" ++ show id

--------------------------------------------------------------------------------

-- | There is a map from ID -> Int and Int -> ID
--   Documentation for each of these is in “Packes.mdown”.
data ID = ACCEPTJOIN          -- 0x04
        | PLAYERINFO          -- 0x06
        | SLOTINFO            -- 0x09
        | KICK                -- 0x1C
        | REQJOIN             -- 0x1E
        | LAN_SEARCHGAME      -- 0x2F
        | LAN_GAMEINFO        -- 0x30
        | LAN_CREATEGAME      -- 0x31
        | LAN_REFRESHGAME     -- 0x32
        | LAN_DECREATEGAME    -- 0x33
        | MAPCHECK            -- 0x3D
        | MAPSIZE             -- 0x42
          deriving (Read, Show, Eq, Ord)

instance Binary ID where
  put = putTable idTable
  get = getTable idTable

idTable :: Table Word8 ID
idTable = mkTable
    [ (0x04, ACCEPTJOIN)
    , (0x06, PLAYERINFO)
    , (0x09, SLOTINFO)
    , (0x1C, KICK)
    , (0x1E, REQJOIN)
    , (0x2F, LAN_SEARCHGAME)
    , (0x30, LAN_GAMEINFO)
    , (0x31, LAN_CREATEGAME)
    , (0x32, LAN_REFRESHGAME)
    , (0x33, LAN_DECREATEGAME)
    , (0x3D, MAPCHECK)
    , (0x42, MAPSIZE) ]

--------------------------------------------------------------------------------

data Data = Null
          | Byte   Word8
          | Short  Word16
          | Long   Word32
          | ULong  Word64
          | String ByteString
          | Struct Structure
            deriving (Show)

instance Binary Data where
    put Null       = put (0 :: Word8)
    put (Byte d)   = put d
    put (Short d)  = putWord16le d
    put (Long d)   = putWord32le d
    put (ULong d)  = putWord64le d
    put (String s) = putLazyByteStringNul s
    put (Struct s) = put s

    get = error "cannot get data; use get<type> instead"

-- | Getters for simple data types
getNull, getByte, getShort, getLong, getULong, getString :: Get Data
getNull   = getWord8 >> return Null
getByte   = fmap Byte getWord8
getShort  = fmap Short getWord16le
getLong   = fmap Long getWord32le
getULong  = fmap ULong getWord64le
getString = fmap String getLazyByteStringNul

--------------------------------------------------------------------------------

data Race = Human
          | Orc
          | NightElf
          | Undead
          | Daemon
          | Random
          | Fixed
            deriving (Show, Eq, Ord)

instance Binary Race where
    put = putTable raceTable
    get = getTable raceTable

raceTable :: Table Word8 Race
raceTable = mkTable 
    [ (0x01, Human)
    , (0x02, Orc)
    , (0x04, NightElf)
    , (0x08, Undead)
    , (0x10, Daemon)
    , (0x20, Random)
    , (0x40, Fixed) ]

--------------------------------------------------------------------------------

data Color = Red
           | Blue
           | Cyan
           | Purple
           | Yellow
           | Orange
           | Green
           | Pink
           | Gray
           | LightBlue
           | DarkGreen
           | Brown
             deriving (Show, Enum, Ord, Eq)

instance Binary Color where
    put = putTable colorTable
    get = getTable colorTable

colorTable :: Table Word8 Color
colorTable = mkTable $ zip [0..] (Red `enumFromTo` Brown) 

--------------------------------------------------------------------------------

data Speed = Slow
           | Normal
           | Fast
             deriving (Show, Eq, Ord)

instance Binary Speed where
    put = putTable speedTable
    get = getTable speedTable

speedTable :: Table Word8 Speed
speedTable = mkTable $ zip [0..] [Slow, Normal, Fast]

--------------------------------------------------------------------------------

data Difficulty = Easy
                | Medium
                | Hard
                  deriving (Show, Eq, Ord)

instance Binary Difficulty where
    put = putTable difficultyTable
    get = getTable difficultyTable

difficultyTable :: Table Word8 Difficulty
difficultyTable = mkTable $ zip [0..] [Easy, Medium, Hard]

--------------------------------------------------------------------------------

-- | Data structures
data Structure = Address { ipv4 :: ByteString,
                           port :: Word16 }
               | Slotinfo { pid          :: Word8,
                            downloaded   :: Word8,
                            status       :: Word8,
                            aiControlled :: Bool,
                            teamNumber   :: Word8,
                            color        :: Color,
                            race         :: Race,
                            difficulty   :: Difficulty,
                            handicap     :: Word8
                          }
               | Gameinfo { speed       :: Speed,
                            visibility  :: Word8,
                            fixedTeams  :: Bool,
                            units       :: Word8,
                            mapWidth    :: Word16,
                            mapHeight   :: Word16,
                            mapChecksum :: Word32,
                            mapPath     :: ByteString,
                            hostsName   :: ByteString
                          }
               deriving (Show)

instance Binary Structure where
    put struct@Address {} = do
        putWord16le 2
        putWord16le . fromIntegral $ port struct
        putWord32le . fromMaybe 0 . inet_aton . toString $ ipv4 struct
        putWord64le 0
    
    put struct@Slotinfo {} = do
        put $ pid struct
        put $ downloaded struct
        put $ status struct
        put $ aiControlled struct
        put $ teamNumber struct
        put $ color struct
        put $ race struct
        put $ difficulty struct
        put $ handicap struct
    
    put struct@Gameinfo {} = do
        let decoded = runPut $ do
            put $ speed struct
            put $ visibility struct
            put $ fixedTeams struct
            put $ units struct
            put 0
            put $ mapWidth struct
            put $ mapHeight struct
            put $ mapChecksum struct
            putLazyByteStringNul $ mapPath struct
            putLazyByteStringNul $ hostsName struct
            put (0 :: Word8)
        putLazyByteString (Util.encode decoded)
        
    get = error "use get<structure> instead"

getAddress, getSlotinfo, getGameinfo :: Get Data
getAddress = do
    port <- get
    ipv4 <- getWord32le
    skip 8
    return $ Struct Address { ipv4 = (fromString . inet_ntoa) ipv4, port = port }
getSlotinfo = do
    pid <- get
    downloaded <- get
    status <- get
    aiControlled <- get
    teamNumber <- get
    color <- get
    race <- get
    difficulty <- get
    handicap <- get
    return $ Struct Slotinfo { pid = pid
                             , downloaded = downloaded
                             , status = status
                             , aiControlled = aiControlled
                             , teamNumber = teamNumber
                             , color = color
                             , race = race
                             , difficulty = difficulty
                             , handicap = handicap }
getGameinfo = do
    encoded <- getLazyByteStringNul
    let decoded = Util.decode encoded
    return $ runGet structure decoded
  where
    structure = do
        speed       <- get
        visibility  <- get
        fixedTeams  <- get
        units       <- get
        skip 1
        mapWidth    <- get
        mapHeight   <- get
        mapChecksum <- get
        mapPath     <- getLazyByteStringNul
        hostsName   <- getLazyByteStringNul
        return $ Struct (mkGameinfo { speed       = speed
                                    , visibility  = visibility
                                    , fixedTeams  = fixedTeams
                                    , units       = units
                                    , mapWidth    = mapWidth
                                    , mapHeight   = mapHeight
                                    , mapChecksum = mapChecksum
                                    , mapPath     = mapPath
                                    , hostsName   = hostsName
                                })
        
--------------------------------------------------------------------------------

-- | Construction of data types

mkW3XP :: Data
mkW3XP = Long 0x57335850

mkPacket :: Packet
mkPacket = Packet { protocol = W3GS KICK,
                     size     = 0,
                     payload  = [] }

mkAddress :: Structure
mkAddress = Address { ipv4 = fromString "127.0.0.1", port = 6112 }

mkSlotinfo :: Structure
mkSlotinfo = Slotinfo { pid          = 0,
                        downloaded   = 100, -- 100%
                        status       = 0,   -- Open
                        aiControlled = False, 
                        teamNumber   = 0,
                        color        = Blue,
                        race         = NightElf,
                        difficulty   = Medium,
                        handicap     = 100
                      }

mkGameinfo :: Structure
mkGameinfo = Gameinfo { speed       = Normal,
                        visibility  = 8,
                        fixedTeams  = True,
                        units       = 0,
                        mapWidth    = 0,
                        mapHeight   = 0,
                        mapChecksum = 0,
                        mapPath     = fromString "Maps\\Download\\DotA Allstars v6.64.w3x",
                        hostsName   = fromString "Puffling"
                      }