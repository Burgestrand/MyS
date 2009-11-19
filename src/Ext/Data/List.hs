module Ext.Data.List (
    module Data.List,
    spliti
) where

import Data.List

{-|
    Split a list into chunks of @n@.
    
    > spliti 3 [1..7]
    > [[1,2,3],[4,5,6],[7]]
-}
spliti :: Int -> [a] -> [[a]]
spliti _ [] = []
spliti n xs = x : spliti n xs'
              where (x, xs') = splitAt n xs