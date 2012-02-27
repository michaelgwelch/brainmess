module Execut_Test where

import Data.IORef
import Jumpable
import Program
import Tape
import Execute
import Test.HUnit

{-
contextEqual :: IO (Program, Tape) -> IO (Program, Tape) -> IO Bool
contextEqual io1 io2 = do
                          (p1,t1) <- io1
                          (p2,t2) <- io2
                          return (p1==p2 && t1==t2)
-}

p = createProgram ""

t :: Tape
t = tape


-- Allows us to mock output. The written char
-- will be stored in the IORef
out :: IORef Char -> Char -> IO ()
out io c = writeIORef io c

-- Set up our input so it returns the char '3'
-- Expect that 51 the ord value of '3' is on the tape
testInput = TestCase $
    do
         (p',t') <- execute ',' p t (return '3') (\_ -> return ()) 
         assertEqual "Expect tape to be [*51*]" (set t 51) t' 

-- use newIORef to create a mutable char 
-- execute the '.' while the current cell is 65, using our IORef for output
-- extract out the value from our IORef
-- Expect it to be the character 'A'
testOutput = TestCase $
    do
         out <- newIORef '_'
         (_,_) <- execute '.' p (set tape 65) (return ' ') (writeIORef out)
         val <- readIORef out
         assertEqual "Expect 'A' which is 65" 'A' val
         
