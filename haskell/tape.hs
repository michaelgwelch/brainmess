module Tape where

-- naive approach that just uses a regular list.

data ListTraveler = Iterate [Int] Int deriving (Show, Eq)

traveler :: ListTraveler
traveler = Iterate [0] 0

moveNext :: ListTraveler -> ListTraveler 
moveNext (Iterate xs pos) | pos >= ((length xs) - 1) = (Iterate (xs ++ [0]) (pos + 1))
                          | otherwise = Iterate xs (pos + 1)

movePrev :: ListTraveler -> ListTraveler 
movePrev (Iterate xs pos) | pos == 0 = Iterate (0 : xs) 0
                          | otherwise = Iterate xs (pos - 1)

getCurrent :: ListTraveler -> Int
getCurrent (Iterate xs pos) = xs !! pos

setCurrent :: ListTraveler -> Int -> ListTraveler
setCurrent (Iterate xs pos) val | pos == 0 = Iterate (val : tail xs) pos
                                | otherwise = 
                                    Iterate (prefix ++ [val] ++ suffix) pos
                                  where prefix = take pos xs
                                        suffix = drop (pos+1) xs


data Tape = Tape ListTraveler deriving (Show, Eq)

createTape :: [Int] -> Int -> Tape
createTape is pos = Tape (Iterate is pos)
tape :: Tape
tape = Tape traveler

inc :: Tape -> Tape
inc (Tape t) = Tape (setCurrent t (getCurrent t + 1))

dec :: Tape -> Tape
dec (Tape t) = Tape (setCurrent t (getCurrent t - 1))

get :: Tape -> Int
get (Tape t) = getCurrent t

set :: Tape -> Int -> Tape
set (Tape t) val = Tape $ setCurrent t val

moveF :: Tape -> Tape
moveF (Tape t) = Tape $ moveNext t

moveR :: Tape -> Tape
moveR (Tape t) = Tape $ movePrev t
