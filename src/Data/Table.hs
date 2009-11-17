module Data.Table where

-- For binary instances
import Data.Binary (Binary, Get, Put, put, get)

import Data.Map (Map)
import qualified Data.Map as Map

-- | Table type
type Table k v = (Map k v, Map v k)

-- | Create a table
mkTable :: (Ord k, Ord a) => [ (k, a) ] -> Table k a
mkTable table = (Map.fromList table, Map.fromList . map switch $ table)
    where switch (a, b) = (b, a)

-- | From a key, find its' value
lookupKey :: (Ord k) => Table k v -> k -> Maybe v
lookupKey (map, _) k = Map.lookup k map

-- | From a value, find its' key
lookupValue :: (Ord v) => Table k v -> v -> Maybe k
lookupValue (_, map) v = Map.lookup v map

-- | Binary put value
putTable :: (Binary k, Ord a, Show a) => Table k a -> a -> Put
putTable table value = 
  case lookupValue table value of
    Just n  -> put n
    Nothing -> error $ "no key for value " ++ show value

-- | A binary get value
getTable :: (Binary k, Ord k, Show k) => Table k a -> Get a
getTable table = do
      num <- get
      case lookupKey table num of
        Just n  -> return n
        Nothing -> error $ "no value for key " ++ show num