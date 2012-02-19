module Tape(Tape(), tape, createTape, moveF, moveR, get, set, inc, dec) where

import Data.Sequence 
import Prelude hiding (length)

data Tape = Tape (Seq Int) Int deriving (Show, Eq)

tape :: Tape
tape = Tape (singleton 0) 0

createTape :: [Int] -> Tape
createTape = (flip Tape) 0 . fromList

moveF :: Tape -> Tape 
moveF (Tape xs pos) | pos >= ((length xs) - 1) = (Tape (xs |> 0) (pos + 1))
                    | otherwise = Tape xs (pos + 1)

moveR :: Tape -> Tape 
moveR (Tape xs 0) = Tape (0 <| xs) 0
moveR (Tape xs pos) = Tape xs (pos - 1)

get :: Tape -> Int
get (Tape xs pos) = index xs pos

set :: Tape -> Int -> Tape
set (Tape xs pos) val = (Tape (update pos val xs) pos)

inc :: Tape -> Tape
inc t = set t $ (get t) + 1

dec :: Tape -> Tape
dec t = set t $ (get t) - 1

