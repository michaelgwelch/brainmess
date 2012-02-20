-- | Represents an infinite tape which is used as a memory story while
--   interpretting a Brainmess program. The tape is a sequence of cells
--   each of which contain a number. The tape also has a /cursor/ which
--   keeps track of the /current/ cell. The current cell is the one that
--   all of the functions in this module operate on.
module Tape(Tape(), tape, createTape, moveF, moveR, get, set, inc, dec) where

import Data.Sequence as Seq
import Data.Foldable (toList)

-- | Represents an infinte list of cells containing numbers.
data Tape = Tape (Seq Int) Int deriving (Eq)

-- | A default tape whose cells are all initialized to 0.
tape :: Tape
tape = Tape (singleton 0) 0

-- | Creates a tape and initializes a portion of it to the specified
--   list and index value. The rest of the cells will still be zero.
--   @'createTape' [2,4,6,8] 2@ creates a tape that contains
--   ..., 0, 0, 2, 4, *6*, 8, 0,... where the *6* indicates the current
--   cell.
createTape :: [Int] -> Int -> Tape  
createTape xs p | p < 0 = error "createTape: p is less than 0"
                | p >= (Prelude.length xs) = error "createTape: p is too large"
                | otherwise = Tape (fromList xs) p

-- | Moves the /cursor/ forward one cell.
moveF :: Tape -> Tape 
moveF (Tape xs pos) | pos >= ((Seq.length xs) - 1) = (Tape (xs |> 0) (pos + 1))
                    | otherwise = Tape xs (pos + 1)

moveR :: Tape -> Tape 
moveR (Tape xs 0) = Tape (0 <| xs) 0
moveR (Tape xs pos) = Tape xs (pos - 1)

-- | Gets the value of the cell at the /cursor/.
get :: Tape -> Int
get (Tape xs pos) = index xs pos

-- | Sets the value of the cell at the /cursor/ to the specified value.
set :: Tape -> Int -> Tape
set (Tape xs pos) val = (Tape (update pos val xs) pos)

-- | Increments the current cell by 1.
inc :: Tape -> Tape
inc t = set t $ (get t) + 1

-- | Decrements the current cell by 1.
dec :: Tape -> Tape
dec t = set t $ (get t) - 1


instance Show Tape where
    show (Tape xs p) = "At index " ++ show p ++ " in " ++ show (toList xs)
