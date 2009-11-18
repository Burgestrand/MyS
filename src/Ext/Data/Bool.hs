module Ext.Data.Bool (
    module Data.Bool,
    (?), (??)
) where

import Data.Bool

-- | Ternary operator
--
-- > \> (1 \> 0) ? 1 $ 0
-- > 1
(?) :: Bool -> a -> a -> a
True ? x = const x
False ? _ = id
 
-- | Higher order ternary operator
--
-- > \> (not . isNothing) ?? Nothing $ Just 1
-- > Just 1
(??) :: (a -> Bool) -> a -> a -> a
f ?? x = f x ? x