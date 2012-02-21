-- | Defines methods for jumping forward and backward on types that 
--   keep track of a current state.
module Jumpable where

-- | Defines jump methods for a data structure that maintains a state.
class Jumpable a where
    -- | Jumps the state forward
    jumpF :: a -> a

    -- | Jumps the state backward
    jumpB :: a -> a
