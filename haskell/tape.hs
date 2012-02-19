module Tape(Tape(), tape, createTape, moveF, moveR, get, set, inc, dec) where

import Data.Sequence as Seq
import Data.Foldable (toList)

data Tape = Tape (Seq Int) Int deriving (Eq)

tape :: Tape
tape = Tape (singleton 0) 0

createTape :: [Int] -> Int -> Tape
createTape xs p | p < 0 = error "createTape: p is less than 0"
                | p >= (Prelude.length xs) = error "createTape: p is too large"
                | otherwise = Tape (fromList xs) p

moveF :: Tape -> Tape 
moveF (Tape xs pos) | pos >= ((Seq.length xs) - 1) = (Tape (xs |> 0) (pos + 1))
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


instance Show Tape where
    show (Tape xs p) = "At index " ++ show p ++ " in " ++ show (toList xs)
