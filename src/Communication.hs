module Communication where

-- | The various message types used for cross-thread communication
data Messages = Command String
              | Disconnect String
              | Packet String
    deriving (Show, Read, Eq)