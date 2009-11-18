{-|
    Common exports a lot of functionality that is common to all subparts of My.
-}
module Common (
    -- * Simplified interfaces
    module Util.Log,
    module Communication,
    module Configuration,
    -- * Libraries & Misc
    module Util.Ternary,
    module Data.Maybe
) where

import Communication
import Configuration
import Data.Maybe
import Util.Log
import Util.Ternary