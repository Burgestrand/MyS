module Util.Ternary where

-- | Ternary operator; usage: @(i > 0) ? i $ 1@
(?) :: Bool -> a -> a -> a
True ? x = const x
False ? _ = id
 
-- | Higher order ternary operator; usage: @(not . null) ?? \"\" $ \"default\"@
(??) :: (a -> Bool) -> a -> a -> a
f ?? x = f x ? x