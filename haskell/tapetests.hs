module Tape_Test where

import Tape
import Test.HUnit

{-
assertEqual :: (Eq a, Show a)  => a -> a -> String -> IO ()
assertEqual x y s | x == y = return ()
                  | otherwise = error $ "Expected: " ++ (show x) ++ ", Actual: "
                        ++ (show y) ++ " - " ++ s

-}

testIncWithDefaultTape = TestCase $ assertEqual
    "Should get [1] 0 when we increment default tape" 
    (createTape [1] 0) (inc tape)

testIncWithPreloadedTape = TestCase $ assertEqual
    "Should see the current cell incremented"
    (createTape [2,7,11,15,17] 2)
    (inc $ createTape [2,7,10,15,17] 2)

incCases = TestList [testIncWithDefaultTape, testIncWithPreloadedTape]

testDecWithDefaultTape = TestCase $ assertEqual
    "Should get [(-1)] 0 when we increment default tape" 
    (createTape [(-1)] 0) (dec tape)

testDecWithPreloadedTape = TestCase $ assertEqual
    "Should see the current cell incremented"
    (createTape [2,7,11,15,17] 2)
    (dec $ createTape [2,7,12,15,17] 2)

decCases = TestList [testDecWithDefaultTape, testDecWithPreloadedTape]


testMoveFWithDefaultTape = TestCase $ assertEqual
    "Expect index to move forward from 0 to 1"
    (createTape [0,0] 1)
    (moveF tape)

testMoveFWithPreloadedTape = TestCase $ assertEqual
    "Expect index to mvoe forward from 2 to 3"
    (createTape [1,7,9,11] 3)
    (moveF $ createTape [1,7,9,11] 2)

moveFCases = TestList [testMoveFWithDefaultTape, testMoveFWithPreloadedTape]

testMoveRWithDefaultTape = TestCase $ assertEqual
    "Expect index to stay at 0 as a new element added at front."
    (createTape [0,0] 0)
    (moveR tape)

testMoveRWithPreloadedTape = TestCase $ assertEqual
    "Expect index to move forward from 3 to 2."
    (createTape [1,7,9,11] 2)
    (moveR $ createTape [1,7,9,11] 3)

moveRCases = TestList [testMoveRWithDefaultTape, testMoveRWithPreloadedTape]

testGetCurrentWithDefaultTape = TestCase $ assertEqual
    "Expect to return 0 from get from default tape"
    0 (get tape)

testGetCurrentWithPreloadedTape = TestCase $ assertEqual
    "Expect to return 5 from get [3,4,5,9] 2"
    5 (get $ createTape [3,4,5,9] 2)

getCases = TestList[testGetCurrentWithDefaultTape, testGetCurrentWithPreloadedTape]


testSetCurrentWithDefaultTape = TestCase $ assertEqual
    "Set current value to 100"
    (createTape [100] 0)
    (set tape 100)

testSetCurrentWithPreloadedTape = TestCase $ assertEqual
    "Set current value to 200"
    (createTape [7,9,200,3] 2)
    (set (createTape [7,9,0,3] 2) 200)

setCases = TestList [testSetCurrentWithDefaultTape, testSetCurrentWithPreloadedTape]

allCases = TestList [incCases, decCases, moveFCases, moveRCases, getCases,
                     setCases]

main = runTestTT allCases


