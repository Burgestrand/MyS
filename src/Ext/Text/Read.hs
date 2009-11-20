module Ext.Text.Read where

import Data.Maybe

-- | Try to 'read' a string
readm :: (Read a) => String -> Maybe a
readm = fmap fst . listToMaybe . reads

-- | 'read' a string, or resort to a default value if unparsable
readDefault :: (Read a) => a -> String -> a
readDefault d = fromMaybe d . readm

-- | 'read' an input from the user and try to parse it
readLnm :: (Read a) => IO (Maybe a)
readLnm = fmap readm getLine

-- | 'read' input, resort to default value if unparsable
readDefaultLn :: (Read a) => a -> IO a
readDefaultLn d = fmap (readDefault d) getLine