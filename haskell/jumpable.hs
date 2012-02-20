-- ! Defines methods for jumping forward and backward on a data structure.
module Jumpable where
class Jumpable a where
    jumpF :: a -> a
    jumpB :: a -> a
