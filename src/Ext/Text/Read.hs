module Ext.Text.Read where

import Data.Maybe

-- | Try to read a string
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Read a string, or resort to a default value if unparsable
readDefault :: (Read a) => a -> String -> a
readDefault d = fromMaybe d . maybeRead