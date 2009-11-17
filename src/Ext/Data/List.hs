module Ext.Data.List where

-- | Splits a list in chunks by index
spliti :: Int -> [a] -> [[a]]
spliti _ [] = []
spliti n xs = x : spliti n xs'
              where (x, xs') = splitAt n xs